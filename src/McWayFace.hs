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


-- Turn off inline-C debugging
-- C.initializeMcWayFaceCtxAndIncludes

data McwServer = McwServer { _msWlDisplay   :: DisplayServer
                           , _msWlEventLoop :: EventLoop
                           , _msBackend     :: Ptr Backend
                           , _msNewOutput   :: WlListener WlrOutput
                           , _msOutputs     :: TVar [McwOutput]
                           }

data McwOutput = McwOutput { _moWlrOutput :: Ptr WlrOutput
                           , _moServer    :: McwServer
                           , _moLastFrame :: TVar TimeSpec
                           , _moColor     :: Color
                        -- , _moDec       :: CInt -- We remove fancy color changing from this demo
                           , _moDestroy   :: WlListener WlrOutput
                           , _moFrame     :: WlListener WlrOutput
                           }

outputFrameNotify :: McwOutput -> (WlListener WlrOutput)
outputFrameNotify mcwOutput = WlListener $ \ptrWlrOutput ->
  do ptrBackend  <- (outputGetBackend ptrWlrOutput) :: IO (Ptr Backend)
     ptrRenderer <- backendGetRenderer ptrBackend
     let color = (_moColor mcwOutput)
     now <- getTime Realtime

     -- Draw color
     makeOutputCurrent ptrWlrOutput
     doRender ptrRenderer ptrWlrOutput $ do rendererClear ptrRenderer color
                                            swapOutputBuffers ptrWlrOutput Nothing Nothing

     -- Update frame timestamp
     atomically $ writeTVar (_moLastFrame mcwOutput) now


outputDestroyNotify :: McwServer -> McwOutput -> WlListener WlrOutput
outputDestroyNotify mcwServer mcwOutput = WlListener $ \_ ->
  do
  -- The purpose of this callback is twofold:
  --   1. Remove the McWOutput from the McwServer's [McwOutput]
  --   2. Destroy the output's WlListener's.
  -- Yet because WlListener's have finalizers attached to them (see Signal.hsc),
  -- we can skip (2) and let the garbage collector do it for us.
  outputList <- atomically $ readTVar (_msOutputs mcwServer)
  let outputListCleansed = removeMcwOutputFromList mcwOutput outputList
  atomically $ writeTVar (_msOutputs mcwServer) outputListCleansed

  where removeMcwOutputFromList mcwOutput list = filter (not . hasSameOutput mcwOutput) list
        hasSameOutput mcwOutput1 mcwOutput2 = (_moWlrOutput mcwOutput1) == (_moWlrOutput mcwOutput2)

newOutputNotify :: McwServer -> WlListener WlrOutput
newOutputNotify mcwServer = WlListener $ \ptrWlrOutput ->
  do
    -- Automatically set the output's width, height, and refresh rate.
    setOutputModeAutomatically ptrWlrOutput

    -- Construct an McwOutput
    now <- getTime Realtime
    nowTvar <- atomically (newTVar now)
    let mcwOutput = McwOutput { _moServer = mcwServer
                              , _moWlrOutput = ptrWlrOutput
                              , _moColor = Color 0.6 0.2 0.8 1 -- purple
                              , _moLastFrame = nowTvar
                              , _moDestroy = (outputDestroyNotify mcwServer mcwOutput)
                              , _moFrame = (outputFrameNotify mcwOutput)
                              }

    -- Connect "destroy" and "frame" callbacks to their appropriate signals.
    -- (HsRoots uses finalizers behind the scenes; no need for us to manually
    -- clean up any memory elsewhere in the program like we do in C.)
    let outputSignals = getOutputSignals ptrWlrOutput
    let destroySignal = (outSignalDestroy outputSignals) :: Ptr (WlSignal WlrOutput)
    let frameSignal  = outSignalFrame outputSignals
    addListener (_moDestroy mcwOutput) destroySignal
    addListener (_moFrame   mcwOutput) frameSignal

    -- Add mcwOutput to the head of SimulaServer's _msOutputs list.
    outputList <- atomically $ readTVar (_msOutputs mcwServer)
    atomically $ writeTVar (_msOutputs mcwServer) (mcwOutput:outputList)

    where setOutputModeAutomatically ptrWlrOutput = do
            hasModes' <- hasModes ptrWlrOutput
            when hasModes' $ do listOfPtrOutputMode <- getModes ptrWlrOutput
                                let headMode = head listOfPtrOutputMode
                                setOutputMode headMode ptrWlrOutput
                                return ()