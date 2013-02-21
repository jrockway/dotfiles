{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import XMonad
import System.Exit
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.NoBorders
import XMonad.Actions.SpawnOn
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen)
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Util.Cursor
import XMonad.Util.Run
import AlmostFull

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

userBindings modMask = [
  ((controlMask .|. mod1Mask, xK_l), spawn "xscreensaver-command -lock"),
  ((modMask, xK_grave), spawn "nyxmms2 toggle")]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawnHere $ XMonad.terminal conf)

    , ((modMask, xK_Delete), (raiseMaybe . spawn) "emacsclient -c -a '' -n" (className =? "Emacs"))
    , ((modMask, xK_End)   , runOrRaise "google-chrome" (className =? "Google-chrome"))
    , ((modMask, xK_Next)  , raiseNext (className =? "URxvt"))
    , ((modMask, xK_Prior) , raiseNext (className =? "MPlayer"))

    -- launch arbitrary programs
    , ((modMask,               xK_p     ), shellPromptHere promptConfig)

    -- xmonad prompt
    , ((modMask .|. shiftMask, xK_p     ), xmonadPrompt promptConfig)

    -- window prompt
    , ((modMask, xK_semicolon                   ), windowPromptGoto  promptConfig)
    , ((modMask .|. shiftMask, xK_semicolon     ), windowPromptBring promptConfig)

    -- xmmsjump
    , ((modMask, xK_i),  xmmsCompletionPrompt)

    -- ssh
    , ((modMask, xK_o), sshPrompt promptConfig)

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

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    ++
    userBindings modMask


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

myLayout = commonManagers $
           tiled |||
           AlmostFull (5/9) delta tiled |||
           Full
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100
     commonManagers = layoutHints . smartBorders . avoidStruts
-- Emacs syntax higlights lines with --> on it like comments, which annoys me.
(=->) = (-->)

myManageHook = composeAll
    [ className =? "mplayer2"          =-> doFloat
--    , className =? "Gimp"              =-> doFloat
    , className =? "Exe"               =-> doFloat
    , resource  =? "desktop_window"    =-> doIgnore
    , className =? "Unity-2d-launcher" =-> doIgnore
    , className =? "Unity-2d-panel"    =-> doIgnore
    , isFullscreen                     =-> doFloat
    ]

myStartupHook = do
  setDefaultCursor xC_left_ptr
  spawn "xsetroot -solid black"
  spawn "xscreensaver -nosplash"
  spawn "xmobar .xmonad/mobar.conf"
  spawn "redshift -l 40.7142:74.0064"
  return ()

myXConfig = XConfig { terminal           = "urxvt"
                    , focusFollowsMouse  = True
                    , borderWidth        = 1
                    , modMask            = mod4Mask
                    , workspaces         = show <$> [1..9]
                    , normalBorderColor  = "#202020"
                    , focusedBorderColor = "#ff0000"
                    , keys               = myKeys
                    , mouseBindings      = myMouseBindings
                    , layoutHook         = myLayout
                    , manageHook         = myManageHook <+> manageDocks <+> manageSpawn
                    , startupHook        = myStartupHook
                    , handleEventHook    = fullscreenEventHook
                    , logHook            = dynamicLogString xmobarPP >>= xmonadPropLog
                    }

main = xmonad $ ewmh myXConfig
