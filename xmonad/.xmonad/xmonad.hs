-- IMPORT                                                                    {{{
--------------------------------------------------------------------------------
import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.Run

import XMonad.Layout.FixedColumn
import XMonad.Layout.LimitWindows
import XMonad.Layout.Magnifier
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

import XMonad.Prompt

import qualified DBus as D
import qualified DBus.Client as D
import qualified XMonad.Layout.BoringWindows as B

import System.Exit
import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Ratio ((%))

import System.IO (hClose)

import qualified Codec.Binary.UTF8.String as UTF8

-----------------------------------------------------------------------------}}}
-- MAIN                                                                      {{{
--------------------------------------------------------------------------------
--TODO: move some programs automatically to workspaces
main :: IO ()
main = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  xmonad
    $ withUrgencyHook NoUrgencyHook
    $ ewmh
    $ addDescrKeys' ((myModMask, xK_F1), xMessage) myKeys
    $ myConfig { logHook = dynamicLogWithPP (myLogHook dbus) }

-----------------------------------------------------------------------------}}}
-- GLOBAL VARIABLES                                                          {{{
--------------------------------------------------------------------------------
-- General config
myTerminal     = "urxvt"
myModMask      = mod4Mask
myBorderWidth  = 1
myBrowser      = "google-chrome-stable"
mySpacing :: Int
mySpacing      = 5
myLargeSpacing :: Int
myLargeSpacing = 30
noSpacing :: Int
noSpacing      = 0
prompt         = 20

-- Colours
fg        = "#ebdbb2"
bg        = "#282828"
gray      = "#a89984"
bg1       = "#3c3836"
bg2       = "#505050"
bg3       = "#665c54"
bg4       = "#7c6f64"

green     = "#b8bb26"
darkgreen = "#98971a"
red       = "#fb4934"
darkred   = "#cc241d"
yellow    = "#fabd2f"
blue      = "#83a598"
purple    = "#d3869b"
aqua      = "#8ec07c"
white     = "#eeeeee"

pur2      = "#5b51c9"
blue2     = "#2266d0"

-- Font
myFont = "xft:SpaceMono Nerd Font Mono:" ++ "fontformat=truetype:size=10:antialias=true"

-----------------------------------------------------------------------------}}}
-- LAYOUT                                                                    {{{
--------------------------------------------------------------------------------
myLayouts = renamed [CutWordsLeft 1] . avoidStruts . minimize . B.boringWindows $ perWS

-- layout per workspace
perWS = onWorkspace wsGEN my3FT $
        onWorkspace wsWRK myAll $
        onWorkspace wsMED my3FT $
        onWorkspace wsTMP myFTM $
        myAll -- all layouts for all other workspaces


myFT  = myTile ||| myFull
myFTM = myTile ||| myFull ||| myMagn
my3FT = myTile ||| myFull ||| my3cmi
myAll = myTile ||| myFull ||| my3cmi ||| myMagn

myFull = renamed [Replace "Full"] $ spacing 0 $ noBorders Full
myTile = renamed [Replace "Main"] $ spacing mySpacing $ Tall 1 (3/100) (1/2)
my3cmi = renamed [Replace "3Col"] $ spacing mySpacing $ ThreeColMid 1 (3/100) (1/2)
myMagn = renamed [Replace "Mag"]  $ noBorders $ limitWindows 3 $ magnifiercz' 1.4 $ FixedColumn 1 20 80 10

-----------------------------------------------------------------------------}}}
-- THEMES                                                                    {{{
--------------------------------------------------------------------------------
-- Prompt themes
myPromptTheme = def
  { font              = myFont
  , bgColor           = darkgreen
  , fgColor           = white
  , fgHLight          = white
  , bgHLight          = pur2
  , borderColor       = pur2
  , promptBorderWidth = 0
  , height            = prompt
  , position          = Top
  }

warmPromptTheme = myPromptTheme
  { bgColor           = yellow
  , fgColor           = darkred
  , position          = Top
  }

coldPromptTheme = myPromptTheme
  { bgColor           = aqua
  , fgColor           = darkgreen
  , position          = Top
  }

-----------------------------------------------------------------------------}}}
-- WORKSPACES                                                                {{{
--------------------------------------------------------------------------------
wsGEN = "\xf120"
wsWRK = "\xf268"
wsMED = "\xf001"
wsTMP = "\xf2db"

myWorkspaces :: [String]
myWorkspaces = [wsGEN, wsWRK, wsMED, wsTMP]

-----------------------------------------------------------------------------}}}
-- PROJECTS                                                                  {{{
--------------------------------------------------------------------------------

-----------------------------------------------------------------------------}}}
-- KEYBINDINGS                                                               {{{
--------------------------------------------------------------------------------
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe "zenity --text-info --font=adobe courier"
  hPutStr h (unlines $ showKm x)
  hClose h
  return ()

myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys conf = let
    subKeys :: String -> [(String, NamedAction)] -> [((KeyMask, KeySym), NamedAction)]
    subKeys s ks = subtitle s : mkNamedKeymap conf ks
    in

    subKeys "System"
    [ ("M-q"    , addName "Restart XMonad"             $ spawn "xmonad --restart")
    , ("M-C-q"  , addName "Recompile & restart XMonad" $ spawn "xmonad --recompile && xmonad --restart")
    ]

    ^++^
    subKeys "Spawnables"
    [ ("M-<Return>" , addName "Terminal"         $ spawn myTerminal)
    , ("M-b"        , addName "Browser"          $ spawn myBrowser)
    ]

    ^++^
    subKeys "Actions"
    [
      ("M-p"         , addName "Take a screenshot of the current workspace"   $ spawn "~/.xmonad/scripts/xshot.sh")
    , ("M-S-p"       , addName "Take a screenshot of the selected area"       $ spawn "~/.xmonad/scripts/xshot-select.sh")
    ]


-----------------------------------------------------------------------------}}}
-- MANAGEHOOK                                                                {{{
--------------------------------------------------------------------------------
myManageHook = composeAll
    [ className =? "MPlayer"          --> doFloat
    , className =? "Gimp"             --> doFloat
    , resource  =? "desktop_window"   --> doIgnore
    , className =? "feh"              --> doFloat
    , className =? "Gpick"            --> doFloat
    , role      =? "pop-up"           --> doFloat ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

myManageHook' = composeOne [ isFullscreen -?> doFullFloat ]

-----------------------------------------------------------------------------}}}
-- LOGHOOK                                                                   {{{
--------------------------------------------------------------------------------
myLogHook :: D.Client -> PP
myLogHook dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{F" ++ blue2 ++ "} ") " %{F-}"
    , ppVisible = wrap ("%{F" ++ blue ++ "} ") " %{F-}"
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    , ppHidden = wrap " " " "
    , ppWsSep = ""
    , ppSep = " | "
    , ppTitle = myAddSpaces 25
    }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

myAddSpaces :: Int -> String -> String
myAddSpaces len str = sstr ++ replicate (len - length sstr) ' '
  where
    sstr = shorten len str

-----------------------------------------------------------------------------}}}
-- STARTUPHOOK                                                               {{{
--------------------------------------------------------------------------------
myStartupHook = do
  setWMName "LG3D"
  spawn "feh --bg-scale $HOME/Downloads/bg.jpg"
  spawn "$HOME/.config/polybar/launch.sh"
  spawn "compton -b --config $HOME/.config/compton.conf"
  spawn "crown-cli start 1"

-----------------------------------------------------------------------------}}}
-- CONFIG                                                                    {{{
--------------------------------------------------------------------------------
myConfig = def
  { terminal            = myTerminal
  , layoutHook          = myLayouts
  , manageHook          = placeHook(smart(0.5, 0.5))
      <+> manageDocks
      <+> myManageHook
      <+> myManageHook'
      <+> manageHook def
  , handleEventHook     = docksEventHook
      <+> minimizeEventHook
      <+> fullscreenEventHook
  , startupHook         = myStartupHook
  , focusFollowsMouse   = False
  , clickJustFocuses    = False
  , borderWidth         = myBorderWidth
  , normalBorderColor   = bg
  , focusedBorderColor  = pur2
  , workspaces          = myWorkspaces
  , modMask             = myModMask
  }
-----------------------------------------------------------------------------}}}
