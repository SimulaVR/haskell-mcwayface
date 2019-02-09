{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module McWayFace where

import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.STM

import           Foreign
-- import qualified Language.C.Inline as C
-- import           Debug.C as C
-- import           Debug.Marshal


-- TODO: Remove unneeded hsroot modules
import Graphics.Wayland.Server
import Graphics.Wayland.Internal.Server
import Graphics.Wayland.WlRoots.Compositor
import Graphics.Wayland.WlRoots.Output
import Graphics.Wayland.WlRoots.Surface
import Graphics.Wayland.WlRoots.Backend
import Graphics.Wayland.Signal
import Graphics.Wayland.WlRoots.Render
import Graphics.Wayland.WlRoots.Render.Color
import Graphics.Wayland.WlRoots.OutputLayout
import Graphics.Wayland.WlRoots.Input
import Graphics.Wayland.WlRoots.Seat
import Graphics.Wayland.WlRoots.Cursor
import Graphics.Wayland.WlRoots.XCursorManager
import Graphics.Wayland.WlRoots.XdgShell

import           System.Clock

outputFrameNotify :: WlListener WlrOutput
outputFrameNotify = WlListener $ \ptrWlrOutput ->
  do now <- getTime Realtime
     putStrLn (show now)

newOutputNotify :: WlListener WlrOutput
newOutputNotify = WlListener $ \ptrWlrOutput ->
  do let outputSignals = getOutputSignals ptrWlrOutput
     let frameSignal  = outSignalFrame outputSignals
     addListener outputFrameNotify frameSignal
     return ()