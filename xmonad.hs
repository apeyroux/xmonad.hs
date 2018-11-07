import           Control.Monad
import qualified Data.Map as M
import           System.IO
import           XMonad
import           XMonad.Actions.Search as S
import           XMonad.Actions.Submap as SM
import           XMonad.Actions.Volume
import           XMonad.Config.Azerty
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Tabbed
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Workspace
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Loggers
import           XMonad.Util.Run (safeSpawn, spawnPipe)

{--
http://xmonad.org/xmonad-docs/xmonad/XMonad-Core.html
http://xmonad.org/xmonad-docs/xmonad/src/XMonad-Core.html#XConfig
--}

term :: String
term = "alacritty"
-- term = "st"

browser :: String
browser = "firefox"

amazonfr :: SearchEngine
amazonfr = searchEngine "amazonfr" "http://www.amazon.fr/s/ref=nb_sb_noss_2?url=search-alias%3Daps&field-keywords="

wikifr :: SearchEngine
wikifr = searchEngine "wikifr" "http://fr.wikipedia.org/wiki/Special:Search?go=Go&search="

photos :: SearchEngine
photos = searchEngine "photos" "https://photos.google.com/search/"

diigo :: SearchEngine
diigo = searchEngine "diigo" "https://www.diigo.com/user/apeyroux?snapshot=yes&query="

mapsfr :: SearchEngine
mapsfr = searchEngine "mapsfr" "http://maps.google.fr/maps?q="

youtubesfr :: SearchEngine
youtubesfr = searchEngine "yt" "http://www.youtube.fr/results?search_type=search_videos&search_query="

multiEngine :: SearchEngine
multiEngine = namedEngine "multifr" $ foldr1 (!>) [wikifr
                                                  , amazonfr
                                                  , mapsfr
                                                  , diigo
                                                  , youtubesfr
                                                  , images
                                                  , photos
                                                  , google]

myLayout = tiled
  ||| Mirror tiled
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 2/3
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myLogHook proc = dynamicLogWithPP $ xmobarPP
  { ppOutput  = hPutStrLn proc
  , ppCurrent = currentStyle
  , ppVisible = visibleStyle
  , ppTitle   = titleStyle
  , ppSep = " <fn=1>\xf101</fn> "
  -- , ppLayout  = (\layout -> case layout of
  --     "Tall"        -> "[|]"
  --     "Mirror Tall" -> "[-]"
  --     "ThreeCol"    -> "[||]"
  --     "Tabbed"      -> "[_]"
  --     "Gimp"        -> "[&]"
  --     )
  }
  where
    currentStyle = xmobarColor "#fff" "" . wrap "<fc=#e74c3c><fn=1>\xf105</fn></fc> " " <fc=#e74c3c><fn=1>\xf104</fn></fc>"
    visibleStyle = wrap "(" ")"
    titleStyle   = xmobarColor "#fff" "" . shorten 200 . filterCurly
    filterCurly  = filter (not . isCurly)
    isCurly x = x == '{' || x == '}'

myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    ((modMask .|. shiftMask, xK_m ), workspacePrompt def (windows . W.shift))
  , ((modMask, xK_Up              ), windows W.focusUp)
  , ((modMask, xK_Down            ), windows W.focusDown)
  , ((modMask .|. shiftMask, xK_d ), shellPrompt def)
  , ((modMask, xK_b               ), sendMessage ToggleStruts)
  , ((modMask, xK_d               ), spawn "rofi -modi drun,ssh,run -show drun")
  , ((modMask, xK_x               ), spawn "i3lock")
  , ((modMask, xK_f               ), sendMessage $ Toggle FULL)
  -- , ((modMask, xK_x            ), spawn "zeal")
  , ((modMask, xK_Left            ), sendMessage Shrink)
  , ((modMask, xK_Right           ), sendMessage Expand)
  , ((noModMask, xK_F12           ), spawn "xbacklight -inc 10")
  , ((noModMask, xK_F11           ), spawn "xbacklight -dec 10")
  , ((modMask, xK_s               ), promptSearchBrowser def browser multiEngine)
  , ((modMask .|. shiftMask, xK_s ), selectSearchBrowser browser google)
  ]
  ++
  -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
  [((m .|. modMask, key              ), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip [xK_z, xK_e, xK_r] [0..]
  , (f, m) <- [(W.view, 0            ), (W.shift, shiftMask)]]

initx :: X()
initx = do
  setWMName "LG3D"
  spawn "feh --bg-scale /home/alex/.bg/bg.png"

main :: IO()
main = do
  xmobar <- spawnPipe "xmobar"
  xmonad (cfg xmobar)
  where
    cfg xbar = docks $ def {
      manageHook = manageDocks
               <+> (isFullscreen --> doFullFloat)
               <+> (className =? "Vlc" --> doFloat)
               <+> (className =? "VirtualBox Manager" --> doFloat)
               <+> (className =? "VirtualBox Machine" --> doFloat)
               <+> (className =? "evince-previewer" --> doFloat)
               <+> (className =? "Evince" --> doFloat)
               <+> (className =? "Nylas Mail" --> doFloat)
               <+> (className =? "Nautilus" --> doFloat)
               <+> (className =? "Zeal" --> doFloat)
               <+> (className =? "Spotify" --> doShift "spotify")
               <+> (className =? "firefox" <||> title =? "chrome" --> doShift "www")
               <+> (stringProperty "WM_WINDOW_ROLE" =? "browser" --> doShift "www")
               <+> (stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doFloat)
               <+> (className =? "Gimp" --> doFloat)
               <+> (className =? ".Desktop-Bridge-wrapped" --> doFloat) -- protonmail bridge
               <+> (className =? "ProtonMail Bridge" --> doFloat) -- protonmail bridge
               <+> (className =? "emacs" --> doShift "emacs")
               <+> (className =? "jetbrains-datagrip" --> doFloat)
               <+> (className =? "Pinentry" --> doFloat)
               <+> (className =? "Virt-manager" --> doFloat)
               <+> (className =? "sun-awt-X11-XFramePeer" --> doFloat)
               <+> (className =? "Antidote 9" --> doFloat)
               <+> (className =? "pavucontrol" --> doFloat)
               <+> (title =? "Authy" --> doFloat)
               <+> (title =? "Postman" --> doFloat)
               <+> manageHook def
               <+> manageDocks,
    terminal = term,
    keys = \c -> azertyKeys c
                 <+> keys def c
                 <+> myKeys c,
    layoutHook = smartBorders $ avoidStruts $ mkToggle (NOBORDERS ?? FULL ?? EOT) myLayout,
    startupHook = initx <+> docksStartupHook <+> startupHook def,
    logHook = myLogHook xbar,
    borderWidth = 1,
    normalBorderColor  = "#44475a",
    focusedBorderColor = "#f4d03f",
    workspaces = ["<fn=1><fc=#5dade2>\xf108</fc></fn>",
                  "<fn=1><fc=#f5b041>\xf269</fc></fn>",
                  "<fn=1><fc=#27ae60>\xf1bc</fc></fn>",
                  "<fn=1><fc=#5dade2>\xf121</fc></fn>"] <+> map show [5..10],
    modMask  = mod4Mask
    }
