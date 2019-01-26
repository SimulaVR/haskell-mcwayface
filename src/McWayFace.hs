{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module McWayFace where

import           Control.Concurrent.STM.TVar
import           Control.Monad.STM

import           Foreign
import           Foreign.C.Types
import           Control.Lens
import qualified Language.C.Inline as C
import           Debug.C as C
import           Debug.Marshal

import           System.Clock

C.initializeMcWayFaceCtxAndIncludes

-- |For ease of translation, we use inline-C types for now.
data McwServer = McwServer { _msWlDisplay :: Ptr C'WlDisplay
                           , _msWlEventLoop :: Ptr C'WlEventLoop
                           , _msBackend :: Ptr C'WlrBackend
                           , _msNewOuput :: Ptr C'WlListener -- We use Ptr C'WlListener as opposed to C'WlListener to make FFI easier
                           , _msOutputs :: TVar [McwOutput]
                           } deriving (Eq)

data McwOutput = McwOutput { _moWlrOutput :: Ptr C'WlrOutput
                           , _moServer :: McwServer
                           , _moLastFrame :: TVar TimeSpec
                        -- , _moColor :: (CFloat, CFloat, CFloat, CFloat) -- We jam this color value via inline-C
                        -- , _moDec :: CInt -- We remove fancy color changing from this demo
                           , _moDestroy :: Ptr C'WlListener
                           , _moFrame :: Ptr C'WlListener
                           } deriving (Eq)


outputFrameNotify :: McwOutput -> Ptr C'WlListener -> Ptr () -> IO ()
outputFrameNotify mcwOutput ptrToWlListener ptrVoid = do
  let ptrToWlrOutput = (castPtr ptrVoid) :: Ptr C'WlrOutput
  [C.block| void { struct wlr_output *wlr_output = $(struct wlr_output * ptrToWlrOutput);
	           struct wlr_renderer *renderer = wlr_backend_get_renderer(wlr_output->backend);
                   const float color[4] = {1, 0, 0, 1}; //Make our output red

                   //pretty coloration code omitted
                   wlr_output_make_current(wlr_output, NULL);
                   wlr_renderer_begin(renderer, wlr_output->width, wlr_output->height);
                   wlr_renderer_clear(renderer, (const float *)(&color)); //compiler complains about this second argument
                   wlr_output_swap_buffers(wlr_output, NULL, NULL);
                   wlr_renderer_end(renderer); }|]
  now <- getTime Realtime
  atomically $ writeTVar (_moLastFrame mcwOutput) now


outputDestroyNotify :: McwServer -> McwOutput -> Ptr C'WlListener -> Ptr () -> IO ()
outputDestroyNotify mcwServer mcwOutput ptrToWlListener ptrVoid = do
  let ptrDestroyListener = _moDestroy mcwOutput
  let ptrFrameListener   = _moFrame mcwOutput

  outputList <- atomically $ readTVar (_msOutputs mcwServer)
  let newList = removeElemFromList mcwOutput outputList
  atomically $ writeTVar (_msOutputs mcwServer) newList

  free (_moDestroy mcwOutput)
  free (_moFrame mcwOutput)

  [C.block| void {
    struct wl_listener * destroy = $(struct wl_listener * ptrDestroyListener);
    struct wl_listener * frame   = $(struct wl_listener * ptrFrameListener);
    wl_list_remove(&destroy->link);
    wl_list_remove(&frame->link);
  }|]
  where removeElemFromList element list = filter (\e -> e/=element) list

-- |We use the closure pattern in our wl_notify_func_t's to access McwServer state.
-- |(Compare this to the wl_container_of() trick found in wlroots C code).
newOutputNotify :: McwServer -> Ptr C'WlListener -> Ptr () -> IO ()
newOutputNotify mcwServer ptrToWlListener voidPtr = do
  let ptrToWlrOutput = (castPtr voidPtr) :: Ptr C'WlrOutput

  -- Sets the output's width, height, and refresh rate.
  [C.block| void {
                if (wl_list_length(&$(struct wlr_output * ptrToWlrOutput)->modes) > 0) {
                  struct wlr_output_mode *mode =
                    wl_container_of((&$(struct wlr_output * ptrToWlrOutput)->modes)->prev, mode, link);
                  wlr_output_set_mode($(struct wlr_output * ptrToWlrOutput), mode);
                }}|]

  -- Tried to do this without pointers but didn't work: https://stackoverflow.com/questions/54375088/marshalling-a-struct-from-c-to-haskell-using-inline-c
  -- destroyListener <- [C.exp| struct wl_listener { struct wl_listener listener }|]
  -- frameListener   <- [C.exp| struct wl_listener { struct wl_listener listener }|]

  -- This memory is destroyed in outputDestroyNotify
  ptrDestroyListener <- [C.exp| struct wl_listener* { malloc(sizeof(struct wl_listener)) }|]
  ptrFrameListener   <- [C.exp| struct wl_listener* { malloc(sizeof(struct wl_listener)) }|]

  now <- getTime Realtime
  nowTvar <- atomically (newTVar now)
  
  let mcwOutput = McwOutput { _moServer = mcwServer
                            , _moWlrOutput = ptrToWlrOutput
                         -- , _moColor = (1,0,0,1) -- we jam this value via inline-C
                            , _moLastFrame = nowTvar
                            , _moDestroy = ptrDestroyListener
                            , _moFrame = ptrFrameListener
                            }

  destroyListenerNotifyPtr <- $(C.mkFunPtr [t| Ptr C'WlListener -> Ptr () -> IO () |]) $ outputDestroyNotify mcwServer mcwOutput
  frameListenerNotifyPtr   <- $(C.mkFunPtr [t| Ptr C'WlListener -> Ptr () -> IO () |]) $ outputFrameNotify   mcwOutput

  [C.block| void { 
                  struct wlr_output * wlr_output = $(struct wlr_output * ptrToWlrOutput);
                  struct wl_listener * destroy = $(struct wl_listener * ptrDestroyListener);
                  struct wl_listener * frame   = $(struct wl_listener * ptrFrameListener);
                  wl_notify_func_t output_destroy_notify = $(wl_notify_func_t destroyListenerNotifyPtr);
                  wl_notify_func_t output_frame_notify = $(wl_notify_func_t frameListenerNotifyPtr);

                  destroy->notify = output_destroy_notify;
                  wl_signal_add(&wlr_output->events.destroy, destroy);
                  frame->notify = output_frame_notify;
                  wl_signal_add(&wlr_output->events.frame, frame);
                }|]

  -- Add mcwOutput to the head of SimulaServer's _msOutputs list.
  outputList <- atomically $ readTVar (_msOutputs mcwServer)
  atomically $ writeTVar (_msOutputs mcwServer) (mcwOutput:outputList)