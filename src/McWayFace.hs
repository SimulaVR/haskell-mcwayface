{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module McWayFace where

import           Foreign
import           Foreign.C.Types
import           Control.Lens
import qualified Language.C.Inline as C
import           Debug.C as C
import           Debug.Marshal

import           System.Clock

C.initializeMcWayFaceCtxAndIncludes

-- |I'm starting with inline-C types for now for ease of translation; 
-- |we can change them to their higher level hsroots types as familiarity with
-- |the codebase is gained.
data McwServer = McwServer {  _ssWlDisplay :: Ptr C'WlDisplay
                                 ,	_msWlEventLoop :: Ptr C'WlEventLoop
                                 ,	_msBackend :: Ptr C'WlrBackend
                                 ,	_msNewOuput :: C'WlListener
                                 ,	_msOutputs :: TVar [McwOutput]
                                 }

data McwOutput = McwOutput { _soWlrOuput :: Ptr C'WlrOutput
                                 , _moServer :: McwServer
                                 , _moTimeSpec :: TVar TimeSpec -- Haskellize this
                                 , _moColor :: (CFloat, CFloat, CFloat, CFloat) -- Using this over Vec4 for now
                              -- , _moDec :: CInt -- We remove fancy color changing from this demo
                                 , _moDestroy :: C'WlListener
                                 , _moFrame :: C'WlListener
                              -- , _moLink :: C'WlList -- Not needed
                                 }


outputFrameNotify :: McwOutput -> Ptr C'WlListener -> Ptr () -> IO ()
outputFrameNotify mcwOutput ptrToWlListener ptrVoid = do
  let ptrToWlrOutput = (castPtr ptrVoid) :: Ptr C'WlrOutput
  [C.block| void { struct wlr_output *wlr_output = $(wlr_output * ptrToWlrOutput);
	                 struct wlr_renderer *renderer = wlr_backend_get_renderer(wlr_output->backend);
                   //pretty coloration code omitted
                   wlr_output_make_current(wlr_output, NULL);
                   wlr_renderer_begin(renderer, wlr_output->width, wlr_output->height);
                   wlr_renderer_clear(renderer, (const float *)(&output->color)); //compiler complains about this second argument
                   wlr_output_swap_buffers(wlr_output, NULL, NULL);
                   wlr_renderer_end(renderer); }}|]

  atomitically $ writeTVar (_moFrame mcwOutput) (getTime Realtime)


-- |We use the closure pattern in our wl_notify_func_t's to access SimulaServer state.
-- |(Compare this to the wl_container_of() trick found in wlroots C code).
newOutputNotifyClosure :: McwServer -> Ptr C'WlListener -> Ptr () -> IO ()
newOutputNotifyClosure simulaServer ptrToWlListener voidPtr = do
  let ptrToWlrOutput = (castPtr voidPtr) :: Ptr C'WlrOutput

  -- Sets the output's width, height, and refresh rate.
  [C.block| void {
                if (wl_list_length(&$(wlr_output ptrToWlrOutput)->modes) > 0) {
                  struct wlr_output_mode *mode =
                    wl_container_of((&$(wlr_output ptrToWlrOutput)->modes)->prev, mode, link);
                  wlr_output_set_mode($(wlr_output ptrToWlrOutput), mode);
                }}|]

  -- TODO: Is it this easy? Or do we use malloc? (Compiler will let me know).
  destroyListener <- [C.exp| struct wl_listener { struct wl_listener listener }⟧
  frameListener   <- [C.exp| struct wl_listener { struct wl_listener listener }⟧

  let mcwOutput = Output { _moServer = simulaServer
                        , _moWlrOutput = ptrToWlrOutput
                        , _moColor = (1,0,0,1)
                        , _moTimespec = (getTime Realtime)
                        , _moDestroy = destroyListener
                        , _moFrame = frameListener
                        }
  destroyListenerNotifyPtr <- $(C.mkFunPtr [t| Ptr C'WlListener -> Ptr () -> IO () |]) $ outputDestroyNotify simulaServer mcwOutput
  frameListenerNotifyPtr   <- $(C.mkFunPtr [t| Ptr C'WlListener -> Ptr () -> IO () |]) $ outputFrameNotify   mcwOutput

  -- TODO: Verify this works
  [C.block| void { 
                  struct wlr_output * wlr_output = $(struct wlr_output * ptrToWlrOutput);
                  wl_listener destroy = $(struct wl_listener destroyListener);
                  wl_listener frame   = $(struct wl_listener frameListener);
                  wl_notify_func_t output_destroy_notify = $(wl_notify_func_t destroyListenerNotifyPtr)
                  wl_notify_func_t output_frame_notify = $(wl_notify_func_t frameListenerNotifyPtr)

                  destroy.notify = output_destroy_notify;
                  wl_signal_add(&wlr_output->events.destroy, &destroy); //`&destroy` might need to be `destroy`
                  output->frame.notify = output_frame_notify;
                  wl_signal_add(&wlr_output->events.frame, &frame); //`&frame` might need to be `frame`
                }}|]

  -- Add mcwOutput to the head of SimulaServer's _msOutputs list.
  outputList <- atomically $ readTVar (_msOutputs)
  atomically $ writeTVar (_msOutputs simulaServer) (mcwOutput:outputList)