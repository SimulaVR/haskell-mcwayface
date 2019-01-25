{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

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
          ptrWlrBackend <- [C.block| wl_event_loop* {
                                wlr_backend * backend;
                                backend = wlr_backend_autocreate(server.wl_display, NULL); 
                                assert(backend);
                                return backend} |]

          -- TODO: Is it this easy? Or do we use malloc? (Compiler will let me know).
          listener      <- [C.exp| struct wl_listener { struct wl_listener listener }⟧
          tvarEmptyList = atomically (newTVar [])

          let mcwServer = McwServer { _ssWlDisplay = ptrWlDisplay
                                    ,	_msWlEventLoop = ptrWlEventLoop
                                    ,	_msBackend = ptrWlrBackend
                                    ,	_msNewOuput = listener
                                    ,	_msOutputs = tvarEmptyList
                                    }

          newOutputNotifyPtr <- $(C.mkFunPtr [t| Ptr C'WlListener -> Ptr () -> IO () |]) $ newOutputNotify mcwServer

          [C.block| void { struct wl_display wl_display = $(struct wl_display * ptrWlDisplay);
                          struct wlr_backend * backend = $(struct wlr_backend * ptrWlrBackend);
                          struct wlr_output * wlr_output = $(struct wlr_output * ptrToWlrOutput);
                          wl_listener listener = $(struct wl_listener listener);
                          wl_notify_func_t new_output_notify = $(wl_notify_func_t newOutputNotifyPtr)

                          listener.notify = new_output_notify;
                                wl_signal_add(&backend->events.new_output, &listener);

                          if (!wlr_backend_start(backend)) {
                            fprintf(stderr, "Failed to start backend\n");
                            wl_display_destroy(wl_display);
                            return 1;
                          }

                          wl_display_run(wl_display);
                          wl_display_destroy(wl_display);}}|]
          return ()