{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent.STM.TVar
import           Control.Monad.STM

import           Foreign
import           Foreign.C.Types
import           Control.Lens
import qualified Language.C.Inline as C
import           Debug.C as C
import           Debug.Marshal

import           System.Clock

import           McWayFace

C.initializeMcWayFaceCtxAndIncludes

main :: IO ()
main = do ptrWlDisplay <- [C.block| struct wl_display* {
                              struct wl_display * display;
                              display = wl_display_create();
                              assert(display);
                              return display;} |]
          ptrWlEventLoop <- [C.block| struct wl_event_loop* {
                                struct wl_event_loop * loop;
                                loop = wl_display_get_event_loop($(struct wl_display *ptrWlDisplay));
                                assert(loop);
                                return loop;} |]
          ptrWlrBackend <- [C.block| struct wlr_backend * {
                                struct wlr_backend * backend;
                                backend = wlr_backend_autocreate($(struct wl_display * ptrWlDisplay), NULL); 
                                assert(backend);
                                return backend;} |]

          -- This memory is destroyed at the end of our main function (see the inline-C snippet).
          ptrNewOutputListener      <- [C.exp| struct wl_listener * { malloc(sizeof(struct wl_listener)) }|]
          tvarEmptyList <- atomically (newTVar [])

          let mcwServer = McwServer { _msWlDisplay = ptrWlDisplay
                                    , _msWlEventLoop = ptrWlEventLoop
                                    , _msBackend = ptrWlrBackend
                                    , _msNewOuput = ptrNewOutputListener
                                    , _msOutputs = tvarEmptyList
                                    }

          newOutputNotifyPtr <- $(C.mkFunPtr [t| Ptr C'WlListener -> Ptr () -> IO () |]) $ newOutputNotify mcwServer

          [C.block| void { struct wl_display * wl_display = $(struct wl_display * ptrWlDisplay);
                           struct wlr_backend * backend = $(struct wlr_backend * ptrWlrBackend);
                           struct wl_listener * ptr_new_output_listener = $(struct wl_listener * ptrNewOutputListener);
                           wl_notify_func_t new_output_notify = $(wl_notify_func_t newOutputNotifyPtr);

                           ptr_new_output_listener->notify = new_output_notify;
                           wl_signal_add(&backend->events.new_output, ptr_new_output_listener);

                           if (!wlr_backend_start(backend)) {
                             fprintf(stderr, "Failed to start backend\n");
                             wl_display_destroy(wl_display);
                             return;
                           }

                           wl_display_run(wl_display);
                           wl_display_destroy(wl_display);
                           free(ptr_new_output_listener);
                           }|]
          return ()