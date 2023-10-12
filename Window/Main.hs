module Main where

import Graphics.UI.Gtk

main :: IO ()
main =
    do
        initGUI
        window <- windowNew
        onDestroy window mainQuit
        widgetShowAll window
        mainGUI
