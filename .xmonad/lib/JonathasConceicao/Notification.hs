-- XMonad audio utility functions
-- Author: Jonathas Conceição
-- https://github.com/Jonathas-Conceicao

module JonathasConceicao.Notification
  (LibNotifyUrgencyHook(..)
  )
  where

import XMonad (windowset, gets)
import XMonad.StackSet (findTag)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedWindows (getName)
import XMonad.Hooks.UrgencyHook ( UrgencyHook
                                , urgencyHook
                                )

data LibNotifyUrgencyHook = LibNotifyUrgencyHook
  deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name     <- getName w
    Just idx <- fmap (findTag w) $ gets windowset
    safeSpawn "notify-send" [show name, "workspace " ++ idx]

