import XMonad
import XMonad.Config.Desktop
import XMonad.Prompt.ConfirmPrompt
import XMonad.Util.Dmenu
import XMonad.Util.Run (spawnPipe)
import XMonad.Prompt.Shell
import XMonad.Layout.ResizableTile
import XMonad.Util.EZConfig (additionalKeysP)
-- import qualified Data.Map as M
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import System.Exit
import Control.Monad

quitWithWarning :: X ()
quitWithWarning = do
    let m = "confirm quit"
    s <- dmenu [m]
    when (s == m) (io exitSuccess)

toggleFloat :: Window -> X ()
toggleFloat w =
  windows
    ( \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else (W.float w (W.RationalRect 0.07 0.07 0.85 0.85) s)
    )

main = xmonad
     . ewmhFullscreen 
     . ewmh 
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPrint)) defToggleStrutsKey 
     $ myConfig

myConfig = desktopConfig
    { terminal = "st tmux"
    , modMask  = mod4Mask
    , borderWidth = 2 
    , normalBorderColor = "black"
    , focusedBorderColor = "gray"
    , layoutHook = ResizableTall 1 (3/100) (1/2) []
    , startupHook = spawn "/home/itsuwari/.config/xmonad/autostart"
    --, keys = newKeys
    }
    `additionalKeysP`
    [ ("M-k", sendMessage MirrorShrink)
    , ("M-j", sendMessage MirrorExpand)
    , ("M-f", spawn "firefox")
    , ("M-d", shellPrompt def)
    , ("<Print>", spawn "xfce4-screenshooter")
    , ("M-S-<Space>", withFocused toggleFloat)
    --, ("M-S-q", quitWithWarning)
    ]

myXmobarPrint :: PP
myXmobarPrint = def
    { ppSep = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 15 

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

--myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
--myKeys conf@(XConfig {XMonad.modMask = mmask}) = M.fromList $ 
--        [ ((mmask, xK_j), sendMessage MirrorShrink)
--        , ((mmask, xK_k), sendMessage MirrorExpand)
--        , ((mmask, xK_f), spawn "firefox")
--        ]
--
--newKeys x = myKeys x `M.union` keys def x
