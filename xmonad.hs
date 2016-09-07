import XMonad
import XMonad.Config.Azerty
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Actions.Volume


{--
http://xmonad.org/xmonad-docs/xmonad/XMonad-Core.html
http://xmonad.org/xmonad-docs/xmonad/src/XMonad-Core.html#XConfig

data XConfig l = XConfig
    { normalBorderColor  :: !String              -- ^ Non focused windows border color. Default: \"#dddddd\"
    , focusedBorderColor :: !String              -- ^ Focused windows border color. Default: \"#ff0000\"
    , terminal           :: !String              -- ^ The preferred terminal application. Default: \"xterm\"
    , layoutHook         :: !(l Window)          -- ^ The available layouts
    , manageHook         :: !ManageHook          -- ^ The action to run when a new window is opened
    , handleEventHook    :: !(Event -> X All)    -- ^ Handle an X event, returns (All True) if the default handler
                                                 -- should also be run afterwards. mappend should be used for combining
                                                 -- event hooks in most cases.
    , workspaces         :: ![String]            -- ^ The list of workspaces' names
    , modMask            :: !KeyMask             -- ^ the mod modifier
    , keys               :: !(XConfig Layout -> M.Map (ButtonMask,KeySym) (X ()))
                                                 -- ^ The key binding: a map from key presses and actions
    , mouseBindings      :: !(XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ()))
                                                 -- ^ The mouse bindings
    , borderWidth        :: !Dimension           -- ^ The border width
    , logHook            :: !(X ())              -- ^ The action to perform when the windows set is changed
    , startupHook        :: !(X ())              -- ^ The action to perform on startup
    , focusFollowsMouse  :: !Bool                -- ^ Whether window entry events can change focus
    , clickJustFocuses   :: !Bool                -- ^ False to make a click which changes focus to be additionally passed to the window
    }
--}

term :: String
term = "gnome-terminal"

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
    ((modMask, xK_d), spawn "dmenu_run")
  , ((modMask, xK_Up), windows W.focusUp)
  , ((modMask, xK_Down), windows W.focusDown)
  , ((modMask, xK_Left), sendMessage Shrink)
  , ((modMask, xK_Right), sendMessage Expand)
  , ((modMask, xK_F12), spawn "xbacklight -inc 10")
  , ((modMask, xK_F11), spawn "xbacklight -dec 10")
  , ((modMask, xK_F2), lowerVolume 3 >> return ())
  , ((modMask, xK_F3), raiseVolume 3 >> return ())
  , ((modMask, xK_F1), toggleMute >> return ())
  ]
  ++
  -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip [xK_z, xK_e, xK_r] [0..]
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

main :: IO()
main = xmonad $ defaultConfig {
  terminal = term,
  keys = \c -> azertyKeys c `M.union` keys defaultConfig c `M.union` myKeys c,
  layoutHook = myLayout,
  borderWidth = 1,
  normalBorderColor  = "#424242",
  focusedBorderColor = "#FA5882",
  -- workspaces = ["emacs", "www", "other"],
  modMask  = mod4Mask
}