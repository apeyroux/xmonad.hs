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
import           XMonad.Layout.NoBorders
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Workspace
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Loggers
import           XMonad.Util.Run (spawnPipe)
import           XMonad.Hooks.SetWMName

{--
http://xmonad.org/xmonad-docs/xmonad/XMonad-Core.html
http://xmonad.org/xmonad-docs/xmonad/src/XMonad-Core.html#XConfig
--}

term :: String
-- term = "terminator"
term = "termite"

browser :: String
browser = "google-chrome-stable"

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

myLayout = tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 2/3
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
  -- ((modMask, xK_Up                ), sendMessage (IncqMasterN 1))
  -- , ((modMask, xK_Down            ), sendMessage (IncMasterN (-1)))
  ((modMask .|. shiftMask, xK_m ), workspacePrompt def (windows . W.shift))
  -- , ((modMask, xK_Up              ), windows W.focusUp)
  -- , ((modMask, xK_Down            ), windows W.focusDown)
  , ((modMask, xK_d               ), shellPrompt def)
  , ((modMask, xK_x               ), spawn "slimlock")
  -- , ((modMask, xK_Left            ), sendMessage Shrink)
  -- , ((modMask, xK_Right           ), sendMessage Expand)
  , ((noModMask, xK_F12           ), spawn "xbacklight -inc 10")
  , ((noModMask, xK_F11           ), spawn "xbacklight -dec 10")
  , ((noModMask, xK_F2            ), lowerVolume 3 >> return ())
  , ((noModMask, xK_F3            ), raiseVolume 3 >> return ())
  , ((noModMask, xK_F1            ), toggleMute >> return ())
  -- Search commands
  , ((modMask, xK_s               ), promptSearchBrowser def browser multiEngine)
  , ((modMask .|. shiftMask, xK_s ), selectSearchBrowser browser google)
  ]
  ++
  -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
  [((m .|. modMask, key              ), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip [xK_z, xK_e, xK_r] [0..]
  , (f, m) <- [(W.view, 0            ), (W.shift, shiftMask)]]

main :: IO()
main = do
  xmonad =<< xmobar def {
    manageHook = manageDocks
               <+> (isFullscreen --> doFullFloat)
               <+> (className =? "Vlc" --> doFloat)
               <+> (className =? "VirtualBox" --> doFloat)
               <+> (className =? "evince-previewer" --> doFloat)
               <+> (className =? "Evince" --> doFloat)
               <+> (className =? "Nylas Mail" --> doFloat)
               <+> (className =? "Nautilus" --> doFloat)
               <+> (className =? "Spotify" --> doShift "spotify")
               <+> (stringProperty "WM_WINDOW_ROLE" =? "browser" --> doShift "www")
               <+> (stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doFloat)
               <+> (className =? "Gimp" --> doFloat)
               <+> (className =? "Pinentry" --> doFloat)
               <+> (className =? "Virt-manager" --> doFloat)
               <+> (className =? "sun-awt-X11-XFramePeer" --> doFloat)
               <+> (className =? "Antidote 9" --> doFloat)
               <+> (className =? "pavucontrol" --> doFloat)
               <+> (title =? "Authy" --> doFloat)
               <+> (title =? "Postman" --> doFloat)
               <+> manageHook def,
    terminal = term,
    keys = \c -> azertyKeys c
                 <+> keys def c
                 <+> myKeys c,
    layoutHook = smartBorders . avoidStruts $ myLayout,
    startupHook = setWMName "LG3D",
    borderWidth = 1,
    normalBorderColor  = "#44475a",
    focusedBorderColor = "#ff79c6",
    workspaces = ["emacs", "www", "spotify", "vbox", "other"],
    modMask  = mod4Mask
}
