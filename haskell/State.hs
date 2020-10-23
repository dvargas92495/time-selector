{-# LANGUAGE DeriveAnyClass, TemplateHaskell #-}

module Poli.Web.Widgets.TimeSelector.State
    ( Action(..)
    , Output(..)
    , State(..)
    , sFocused
    , sFuture
    , sIndex
    , sInput
    , sResults
    , sTime
    , mkState
    ) where

import Data.Time (LocalTime)

import Poli.Web.Base
import qualified Poli.Web.Widgets.Modal.State as M

data State = State
    { _sIndex :: Int
    , _sInput :: MisoString
    , _sResults :: [(MisoString, LocalTime)]
    , _sTime :: Maybe LocalTime
    , _sFocused :: Bool
    , _sFuture :: Bool
    } deriving (Eq, Generic, FromJSON, ToJSON)

makeLenses ''State

data Action
    = Load
    | Focus
    | Modify (State -> State)
    | Output Output
    | SearchTimes MisoString
    | Select Int
    | ModalAction M.Output
    | NoOp

data Output
    = SetState State
    | Close
    | None
    | Success LocalTime

mkState :: Bool -> State
mkState isfuture = State
    { _sIndex = 0
    , _sInput = ""
    , _sResults = []
    , _sTime = Nothing
    , _sFocused = False
    , _sFuture = isfuture
    }
