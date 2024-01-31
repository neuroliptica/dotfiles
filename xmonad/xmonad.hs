-- *
-- | xmonad @neuroliptica's config
-- | xmonad + xmobar + tmux st
-- *

-- | General config 
import XMonad
import XMonad.Config.Desktop
import XMonad.Util.Dmenu
import XMonad.Util.Run (spawnPipe)
import XMonad.Prompt.Shell
import XMonad.Layout.ResizableTile
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Hooks.EwmhDesktops

-- | Status bar
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

-- | Logging
import XMonad.Util.Loggers

import qualified Data.Map as M
-- | Float managing
import qualified XMonad.StackSet as W

import System.Exit
import Control.Monad

import Data.List

quitWithWarning :: X ()
quitWithWarning = do
    let m = "confirm quit"
    s <- dmenu [m]
    when (s == m) (io exitSuccess)

toggleFloat :: Window -> X ()
toggleFloat w = do 
    --(_, rr) <- floatLocation w
    let rr = W.RationalRect 0.07 0.07 0.85 0.85
    windows $ \s ->
        if M.member w (W.floating s)
          then W.sink w s 
          else W.float w rr s

myManageHook = composeAll . concat $
   [ [className =? c --> doFloat | c <- cFloats ] 
   , [title     =? t --> doFloat | t <- tFloats ]
   ]
   where cFloats = []
         tFloats = ["Media viewer"]

main = xmonad
     . ewmhFullscreen 
     . ewmh 
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPrint)) defToggleStrutsKey 
     $ myConfig

myConfig = desktopConfig
    { terminal           = "st tmux"
    , modMask            = mod4Mask
    , borderWidth        = 1
    , normalBorderColor  = "black"
    , focusedBorderColor = "#AA33EB"
    , layoutHook         = ResizableTall 1 (3/100) (1/2) []
    , startupHook        = spawn "/home/itsuwari/.config/xmonad/autostart"
    , manageHook         = myManageHook <+> manageHook def
    }
    `additionalKeysP`
    [ ("M-k", sendMessage MirrorShrink)
    , ("M-j", sendMessage MirrorExpand)
    , ("M-f", spawn "firefox")
    , ("M-d", shellPrompt def)
    , ("<Print>", spawn "xfce4-screenshooter")
    , ("M-S-<Space>", withFocused toggleFloat)
    , ("M-S-q", quitWithWarning)
    ]

myXmobarPrint :: PP
myXmobarPrint = def
    { ppSep = magenta "  "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . blue . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . magenta    . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 15 

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
