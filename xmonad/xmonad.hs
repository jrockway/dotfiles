{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import XMonad
import System.Exit
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.NoBorders
import XMonad.Actions.SpawnOn
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import XMonad.Util.Run

import Control.Applicative
import Control.Monad
import Data.Char (toLower)
import Data.List (isPrefixOf, filter)
import Data.Maybe
import Data.Monoid
import Data.Ratio
import Text.Regex.Posix

import qualified Codec.Binary.UTF8.String as UTF8

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvtcd"

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces = show <$> [1..9]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#202020"
myFocusedBorderColor = "#ff0000"

promptConfig = defaultXPConfig { bgHLight = "DodgerBlue"
                               , fgHLight = "white"
                               , bgColor = "grey70"
                               , fgColor = "black"
                               , font = "xft:DejaVu Sans Mono-30"
                               , promptBorderWidth = 0
                               , height = 42
                               }

-- -- case-insensitive string comparison
-- mkCompletionFunction :: [String] -> String -> IO [String]
-- mkCompletionFunction options [] = return []
-- mkCompletionFunction options query =
--     let toLower' = fmap toLower
--         quotemeta c = case c of
--                         ')' -> "\\)"
--                         '(' -> "\\("
--                         '[' -> "\\["
--                         ']' -> "\\]"
--                         '\\' -> "\\\\"
--                         _   -> c:[]
--         quotemeta' x = join (fmap quotemeta x)
--         query' = toLower' . quotemeta' $ query
--     in return $ filter (\opt -> (toLower' opt) =~ query') options

-- allXmmsCompletions :: X [String]
-- allXmmsCompletions = do
--   result <- runProcessWithInput "sh" ["-c", "/usr/bin/perl `which xmmsjump` --list-all"] ""
--   return . take 20 . lines . UTF8.decodeString $ result

xmmsCompletionPrompt :: X ()
xmmsCompletionPrompt = do
    -- completions <- allXmmsCompletions
    -- let completionFn = mkCompletionFunction completions
    inputPrompt promptConfig "Jump" ?+ \song ->
      spawn $ "/usr/bin/perl `which xmmsjump` " ++ (UTF8.encodeString song)

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawnHere $ XMonad.terminal conf)

    , ((modMask, xK_a), runOrRaise "emacsclient -c" (className =? "Emacs"))
    , ((modMask, xK_s), runOrRaise "conkeror" (className =? "Conkeror"))
    , ((modMask, xK_d), raiseNext (className =? "URxvt"))

    -- launch arbitrary programs
    , ((modMask,               xK_p     ), shellPromptHere promptConfig)

    -- xmonad prompt
    , ((modMask .|. shiftMask, xK_p     ), xmonadPrompt promptConfig)

    -- xmmsjump
    , ((modMask, xK_o),  xmmsCompletionPrompt)

    -- ssh
    , ((modMask, xK_i), sshPrompt promptConfig)

    -- close focused window
    , ((modMask              , xK_q     ), kill)

    -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Move/view empty workspace
    , ((modMask,               xK_n     ), viewEmptyWorkspace)
    , ((modMask .|. shiftMask, xK_n     ), tagToEmptyWorkspace)

    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- toggle the status bar gap
    , ((modMask              , xK_b     ), sendMessage ToggleStruts)

--          modifyGap (\i n -> let x = (XMonad.defaultGaps conf ++ rep--eat (0,0,0,0)) !! i
--                             in if n == x then (0,0,0,0) else x))
--
    -- Restart xmonad
    , ((modMask .|. shiftMask, xK_q     ),
          broadcastMessage ReleaseResources >> restart "xmonad" True)

    -- remove border from window
    , ((modMask, xK_g), withFocused toggleBorder)

    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
--- myLayout = --- layoutHints (smartBorders (avoidStruts (tiled ||| Mirror tiled)))
           --- ||| (smartBorders (avoidStruts Full))

myCommonManagers = layoutHints . smartBorders . avoidStruts

myLayout = myCommonManagers (tiled ||| Mirror tiled ||| OneBig (3/4) (3/4) ||| Full)

  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"           --> doFloat
    , className =? "Gimp"              --> doFloat
    , resource  =? "desktop_window"    --> doIgnore
    ]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--

myLogHook = dynamicLogWithPP $
            defaultPP { ppCurrent = xmobarColor "#c0ffee" "" . wrap "[" "]"
                      , ppTitle   = (\t -> ((xmobarColor "#c0ffee" "" . shorten 150) t))
                      , ppVisible = wrap "(" ")"
                      }

-- | setup a normal session -- somewhat
sessionSetupAction :: XConfig Layout -> X ()
sessionSetupAction conf = do
  spawnOn "1" "emacsclient -c"
  forM_ [1..3] $ \_ -> spawnOn "2" "urxvt"
  spawnOn "2" "conkeror"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
myXConfig = XConfig { terminal           = myTerminal
                    , focusFollowsMouse  = myFocusFollowsMouse
                    , borderWidth        = myBorderWidth
                    , modMask            = myModMask
                    -- , numlockMask        = myNumlockMask
                    , workspaces         = myWorkspaces
                    , normalBorderColor  = myNormalBorderColor
                    , focusedBorderColor = myFocusedBorderColor
                    , keys               = myKeys
                    , mouseBindings      = myMouseBindings
                    , layoutHook         = myLayout
                    , manageHook         = myManageHook <+> manageDocks <+> manageSpawn
                    , logHook            = myLogHook
                    , startupHook        = return ()
                    , handleEventHook    = (\x -> return $ Data.Monoid.All { getAll = True })
                    }

main = xmonad myXConfig
