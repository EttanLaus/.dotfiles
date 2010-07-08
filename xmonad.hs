-- Imports
import XMonad

-- Needed for myManageHook
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
-- Needed for layouthook

import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.NoBorders
import XMonad.Hooks.UrgencyHook
-- Needed for keys
import qualified Data.Map as M
import XMonad.Util.Run

-- Needed for statusbar
import XMonad.Hooks.DynamicLog
import System.IO

-- Needed for prompt
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Actions.NoBorders



myTerminal      = "urxvt"
myBorderWidth   = 1
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#0078CE"

myWorkspaces    = ["1:Main","2:Musika","3:Wine"]

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#ffa402" "#161616" . pad 
      , ppVisible           =   dzenColor "white" "#161616" . pad
      , ppHidden            =   dzenColor "#02a2ff" "#161616" . pad
      , ppHiddenNoWindows   =   dzenColor "#444444" "#161616" . pad 
      , ppUrgent 			=   dzenColor "#FFFFAF" "" . wrap "[" "]"
      , ppWsSep             =   " "
      , ppSep               =   "  |  "
      , ppOrder = \(ws : l : _ : xs) -> ws : l : xs
      , ppLayout            =   dzenColor "#f03669" "#161616" .
                                (\x -> case x of
                                        "Spacing 4 Tall"         ->      "Tiled"
                                        "Spacing 4 Full"         ->      "Full"
                                        "Mirror Spacing 10 Tall"  ->      "Mirror Tall"
                                        "IM ReflectX IM Full"     ->      "IM"
                                        "ReflectX IM Mirror Spacing 10 Tall" ->  "IM"
                                        _                         ->      x
                                )
      , ppOutput            =   hPutStrLn h
    }

myManageHook = composeAll
    [ className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , className =? "stalonetrayer"  --> doIgnore
    ]
    
------------------------------------------------------------------------

myLayout = avoidStruts $ onWorkspace "3:Wine"  wineLayouts
                       $ standardLayouts

    where
    standardLayouts = spacing 4 $ toggleLayouts Full (Tall 1 (3/100) (1/2)) ||| Mirror tiled ||| Full ||| tiled
    wineLayouts      = simplestFloat ||| tiled ||| Mirror tiled ||| smartBorders (Full)
    -- default tiling algorithm partitions the screen into two panes
    tiled  = Tall nmaster delta ratio    
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100
    
------------------------------------------------------------------------
  -- Key mapping {{{

newKeys x = M.union (keys defaultConfig x) (M.fromList (myKeys x))

myKeys conf@(XConfig {XMonad.modMask = modm}) =
             [ ((modm, xK_f), sendMessage (Toggle "Full"))
             , ((modm, xK_c), kill)
             , ((modm, xK_g), spawn $ XMonad.terminal conf)
             , ((modm,               xK_space ), sendMessage NextLayout)
             , ((modm, xK_x), shellPrompt myXPConfig)
             , ((modm,  xK_y ),   withFocused toggleBorder)
             , ((modm              , xK_i     ), spawn "killall conky dzen2 && xmonad --recompile && xmonad --restart")
             ]

----- Prompt -------
myXPConfig :: XPConfig
myXPConfig =
    defaultXPConfig { font                  = "-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*"
                    , bgColor               = "#161616"
                    , fgColor               = "#ebac54"
                    , autoComplete  		= Just 500000
                    }

---Dzen----
myStatusBar = "dzen2 -p -x '0' -y '0' -h '24' -w '1270' -ta 'l' -fg '#FFFFFF' -bg '#151515' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'"
myDateBar = "conky -c /home/andre/.conkyrc | dzen2 -x '600' -y '0' -h '24' -w '900' -ta 'r' -bg '#151515' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'"

main = do
	workspaceBar <- spawnPipe myStatusBar
	bar2 <- spawnPipe myDateBar
	-- h <- spawnPipe myStatusBar
	xmonad  $ withUrgencyHook NoUrgencyHook defaultConfig
		{
      -- simple stuff
		keys			   = newKeys,
        terminal           = myTerminal,
        borderWidth        = myBorderWidth,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        modMask 		   = mod4Mask,
      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook <+> manageDocks <+> manageHook defaultConfig,
        logHook            = myLogHook workspaceBar
    }
