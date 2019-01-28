{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent.STM.TVar
import           Control.Monad.STM

import           Foreign
import           Foreign.C.Types
import           Foreign.C.Error
import           Control.Lens
-- import qualified Language.C.Inline as C
-- import           Debug.C as C
-- import           Debug.Marshal
import           Data.Coerce

import           System.Clock


-- TODO: Remove unneeded hsroot modules
import Graphics.Wayland.Server
import Graphics.Wayland.Internal.Server
import Graphics.Wayland.WlRoots.Compositor
import Graphics.Wayland.WlRoots.Output
import Graphics.Wayland.WlRoots.Surface
import Graphics.Wayland.WlRoots.Backend
import Graphics.Wayland.Signal
import Graphics.Wayland.WlRoots.Render
import Graphics.Wayland.WlRoots.OutputLayout
import Graphics.Wayland.WlRoots.Input
import Graphics.Wayland.WlRoots.Seat
import Graphics.Wayland.WlRoots.Cursor
import Graphics.Wayland.WlRoots.XCursorManager
import Graphics.Wayland.WlRoots.XdgShell

import           McWayFace

-- Turn off inline-C debugging
-- C.initializeMcWayFaceCtxAndIncludes

main :: IO ()
main = do displayServer <- throwIfNullPtr displayCreate
          eventLoop     <- throwIfNullPtr (displayGetEventLoop displayServer)
          ptrBackend    <- backendAutocreate displayServer -- automatically throws if NULL
          tvarEmptyList <- atomically (newTVar [])

          let mcwServer = McwServer { _msWlDisplay = displayServer
                                    , _msWlEventLoop = eventLoop
                                    , _msBackend = ptrBackend
                                    , _msNewOutput = (newOutputNotify mcwServer)
                                    , _msOutputs = tvarEmptyList
                                    }

          let newOutputSignal = (backendEvtOutput $ backendGetSignals ptrBackend) :: Ptr (WlSignal WlrOutput)
          addListener (newOutputNotify mcwServer) newOutputSignal 

          backendStart ptrBackend -- Doesn't destroy the wl_display; potential memory leak
          displayRun displayServer
          displayDestroy displayServer


  where throwIfNullPtr f = throwErrnoIf
                           (\res -> ((coerce res :: Ptr ()) == nullPtr)) -- Throw error if the pointer returned by f is 0 (i.e., NULL)
                           "wl_* initialization failed."
                           f