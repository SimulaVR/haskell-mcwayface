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
                                 ,	_msOutputs :: [Ptr C'WlrOutput]
                                 }

data McwOutput = McwOutput { _soWlrOuput :: Ptr C'WlrOutput
                                 , _moServer :: McwServer
                                 , _moTimeSpec :: TimeSpec -- Haskellize this
                                 , _moColor :: (CFloat, CFloat, CFloat, CFloat) -- Using this over Vec4 for now
                                 , _moDec :: CInt
                                 , _moDestroy :: C'WlListener
                                 , _moFrame :: C'WlListener
                              -- , _moLink :: C'WlList -- Not needed
                                 }

