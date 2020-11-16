-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String
import TimeSelector.View
import TimeSelector.State (mkState, Action( NoOp ))

-- | Type synonym for an application model
type Model = Int

updateModel :: Action -> Model -> Effect Action Model
updateModel action m = noEff m

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp
    model  = 0                    -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in sync (only applies to `miso` function)

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
  TimeSelector.View.view "" (mkState False)
 ]
