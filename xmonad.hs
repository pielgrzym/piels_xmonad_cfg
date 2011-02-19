import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP, removeKeysP)
import System.IO
import qualified XMonad.StackSet as W
import XMonad.Actions.GridSelect
import XMonad.Util.Loggers
import XMonad.Util.Font
import XMonad.Hooks.ManageHelpers ( isFullscreen, isDialog, doCenterFloat, doFullFloat )
import XMonad.Layout.NoBorders
import XMonad.Actions.CycleWS
-- copy windows! tag-like functionality
import XMonad.Actions.CopyWindow
-- extra layouts
import XMonad.Layout.PerWorkspace 
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Layout.Maximize
-- sublayouts
import XMonad.Layout.SubLayouts(GroupMsg(UnMergeAll, UnMerge, MergeAll, SubMessage), defaultSublMap, onGroup, pullGroup, pushWindow, subLayout, subTabbed)
import XMonad.Layout.WindowNavigation
import XMonad.Layout.BoringWindows
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
-- urgency
import XMonad.Hooks.UrgencyHook
-- dmenu fun
import XMonad.Util.Dmenu
-- logers 4 dzen2
import XMonad.Util.Loggers

import XMonad.Layout.Monitor
import XMonad.Layout.LayoutModifier

main= do 
        bar <- spawnPipe myStatusBar
        bar2 <- spawnPipe "/home/pielgrzym/.xmonad/dzen.sh"
        wall <- spawnPipe "nitrogen --restore"
        -- conky_clock <- spawnPipe "killall conky; conky"
        xmonad $ withUrgencyHook NoUrgencyHook
               $ defaultConfig 
                { 
                borderWidth          = 3
                , terminal           = "urxvt"
                , normalBorderColor  = "#008800"
                , focusedBorderColor = myMainColor
                , modMask            = mod4Mask     -- Rebind Mod to the Windows key 
                , workspaces         = myWorkspaces
                , manageHook         = myManageHook <+> manageDocks 
                -- , manageHook         = myManageHook <+> manageDocks <+> manageMonitor clock
                , layoutHook         = myLayout
                , logHook            = dynamicLogWithPP $ myDzenPP bar
                --, logHook            = dynamicLogWithPP $ myXmobarPP bar
                }
                --`removeKeysP` ["M-" ++ [n] | n <- ['1'..'9']]
                --`removeKeysP` ["M-S-" ++ [n] | n <- ['1'..'9']]
                --`removeKeysP` ["M-C-" ++ [n] | n <- ['1'..'9']]
                `removeKeysP` [ "M-w", "M-e", "M-b" ] 
                `additionalKeysP`
                (
                [ ("M-r",       spawn (myDmenu))
                , ("M-g",       goToSelected defaultGSConfig)
                --, ("M-n",     sendMessage MirrorShrink)
                , ("M-n",       nextWS)
                , ("M-p",       prevWS)
                , ("M-u",       focusUrgent)
                , ("M-;",       withFocused (sendMessage . maximizeRestore))
                , ("M-'",       broadcastMessage ToggleMonitor >> refresh)
                -- cmus control
                , ("M-z",       spawn "cmus-remote --prev")
                , ("M-x",       spawn "cmus-remote --play")
                , ("M-c",       spawn "cmus-remote --pause")
                , ("M-e",       spawn "cmus-remote --pause")
                , ("M-v",       spawn "cmus-remote --stop")
                , ("M--",       spawn "cmus-remote --vol -10%")
                , ("M-=",       spawn "cmus-remote --vol +10%")
                -- eof cmus control
                , ("M-S-c",     kill1)  -- remove a window copy or kill window otherwise
                , ("M-M1-k",    sendMessage MirrorExpand)
                , ("M-M1-j",    sendMessage MirrorShrink)
                , ("M-m M-h",   sendMessage $ pullGroup L) -- Merge to Tabbed
                , ("M-m M-l",   sendMessage $ pullGroup R)
                , ("M-m M-k",   sendMessage $ pullGroup U)
                , ("M-m M-j",   sendMessage $ pullGroup D)
                , ("M-m m",     withFocused (sendMessage . MergeAll))
                , ("M-m S-m",   withFocused (sendMessage . UnMergeAll))
                , ("M-S-m",     withFocused (sendMessage . UnMerge))
                , ("M-m M-h",   withFocused (sendMessage . SubMessage (SomeMessage Shrink) ))
                , ("M-m M-l",   withFocused (sendMessage . SubMessage (SomeMessage Expand) ))
                --, ("M-m M-space",   withFocused (sendMessage . SubMessage (SomeMessage NextLayout) ))
                , ("M-S-,",     onGroup W.focusUp') -- Move focus between tabs
                , ("M-S-.",     onGroup W.focusDown') -- Move focus between tabs
                ]
                ++
                -- below: screen swithing with 'i' and 'o'
                [("M-"++m++[key], screenWorkspace sc >>= flip whenJust (windows . f))
                        | (f, m) <- [(W.view, ""), (W.shift, "S-"), (copy, "C-")]
                        , (key, sc) <- zip "io" [0 .. ]]
                ++
                -- below: workspace greedy switch with M-[0..9], move to ws with M-S-[0..9] and copy to ws with M-C-[0..9]
                [("M-"++m++[key], action tag)
                        | (tag, key) <- zip myWorkspaces ['1'..'9']
                        , (action, m) <- [(windows . W.greedyView, ""), (windows . W.shift, "S-"), (windows . copy, "C-")]]
                )

myWorkspaces = ["1:im", "2:local", "3:temp", "4:var", "5:books", "6:music", "7:web", "8:dev", "9:remote"]
myDmenu = "dmenu_run -fn terminus -nf \""++myDzenFGColor++"\" -nb \""++myDzenBGColor++"\" -sb \""++myDzenFGColor++"\" -sf \""++myDzenBGColor++"\""

clock = monitor {
        -- Cairo-clock creates 2 windows with the same classname, thus also using title
                prop = ClassName "urxvt" `And` Title "ZEGAR"
                -- rectangle 150x150 in lower right corner, assuming 1280x800 resolution
                , rect = Rectangle (1600-151) (1200-101) 150 100
                -- avoid flickering
                , persistent = True
                -- make the window transparent
                -- hide on start
                , visible = False
                -- assign it a name to be able to toggle it independently of others
                , name = "clock"
}

-- Color, font and iconpath definitions:
myMainColor = "#00aa00"
myFont = "snap"
myIconDir = "/home/pielgrzym/.xmonad/icons"
myDzenFGColor = myMainColor
myDzenBGColor = "#262626"
myNormalFGColor = "#ffffff"
myNormalBGColor = "#0f0f0f"
myFocusedFGColor = myMainColor
myFocusedBGColor = "#333333"
myUrgentFGColor = "#ffffff"
myUrgentBGColor = myMainColor
myIconFGColor = "#777777"
myIconBGColor = "#0f0f0f"
mySeperatorColor = "#555555"

-- layout hook
myLayout = avoidStruts 
        $ ModifiedLayout clock
        $ smartBorders
        $ windowNavigation
        $ maximize
        $ boringWindows
        $ onWorkspace "1:im" (enableTabs three_col')
        $ onWorkspace "7:web" big_layouts
        $ default_layouts
        where
            default_layouts = (tabbed' ||| enableTabs resizable_tall' ||| enableTabs (Mirror resizable_tall') ||| magni_tall)
            big_layouts = (tabbed' ||| Full ||| magni_tall)
            -- complex layout definitions:
            resizable_tall' = spacing 2 $ ResizableTall 1 (3/100) (1/2) []
            tabbed'        = withBorder 1 $ tabbed shrinkText myTabTheme
            three_col'     = spacing 2 $ ThreeColMid 2 (3/100) (4/5)
            enableTabs x  = addTabs shrinkText myTabTheme $ subLayout [] Simplest x
            magni_tall = magnifier resizable_tall'
            -- two_pane_tall = subLayout [0,1,2,1] (Tall 1 0.2 0.5 ||| tabbed' ||| Circle) $ resizable_tall'

         
-- tabbed theme
myTabTheme = defaultTheme
    { activeColor = "" ++ myDzenFGColor ++ ""
    , inactiveColor = "" ++ myDzenBGColor ++ ""
    , urgentColor = "" ++ myUrgentBGColor ++ ""
    , activeBorderColor = "" ++ myDzenFGColor ++ ""
    , inactiveBorderColor ="" ++ myDzenBGColor ++ ""
    , urgentBorderColor = "" ++ myDzenFGColor ++ ""
    , activeTextColor = "#000000"
    , inactiveTextColor = "" ++ myDzenFGColor ++ ""
    , urgentTextColor = "" ++ myUrgentFGColor ++ ""
    , fontName = "" ++ myFont ++ ""
    --, decoWidth = ""
    , decoHeight = 16
    }

-- manage hook
myManageHook = composeAll
    [ isFullscreen                  --> doFullFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "Smplayer"       --> doFloat
    , className =? "Gimp"           --> doFloat
    , title     =? "ZEGAR"          --> doIgnore
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

-- myStatusBar = "xmobar"
myStatusBar = "dzen2 -x '0' -y '0' -h '12' -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myDzenBGColor ++ "' -fn '" ++ myFont ++ "'"
--myStatusBar = "dzen2 -xs 1 -x '0' -y '0' -h '12' -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myDzenBGColor ++ "' -fn '" ++ myFont ++ "'"
 
myXmobarPP h = defaultPP
    { ppCurrent = wrap ("[<fc=" ++ myUrgentFGColor ++ ">") "</fc>]" . \wsId -> dropIx wsId
    , ppVisible = wrap ("[<fc=" ++ myNormalFGColor ++ ">") "</fc>]" . \wsId -> dropIx wsId
    , ppHidden = wrap "" "" . \wsId -> dropIx wsId -- don't use <fc> here!!
    --, ppHiddenNoWindows = wrap ("<fc=" ++ myDzenFGColor ++ ">") "</fc>" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs then "" else wrap ("<fc=" ++ myDzenFGColor ++ ">") "</fc>" . dropIx $ wsId
    , ppUrgent = wrap ("<fc=" ++ myUrgentFGColor ++ ">!") "!</fc>" . \wsId -> dropIx wsId
    , ppSep = " "
    , ppWsSep = " "
    , ppTitle = xmobarColor (""++ myNormalFGColor ++ "") "" . wrap "< " " >"
    , ppLayout = xmobarColor (""++ myDzenFGColor ++ "") "" .
        (\x -> case x of
        "Full" -> "[ ]"
        "ResizableTall" -> "[|]"
        "Mirror ResizableTall" -> "[-]"
        "Tabbed Simplest" -> "[T]"
        "ThreeCol" -> "[3]"
        "OneBig 0.75 0.75" -> "[B]"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
    dropIx wsId = if (':' `elem` wsId) then drop 2 wsId else wsId
    staticWs = ["1:im", "2:local", "7:web", "8:dev", "9:remote"]

myDzenPP h = defaultPP
    { ppCurrent = dzenColor myFocusedFGColor myFocusedBGColor . dzenIcon "has_win.xbm" . \wsId -> dropIx wsId
    , ppVisible = dzenColor myNormalFGColor myNormalBGColor  . dzenIcon "has_win.xbm" . \wsId -> dropIx wsId
    , ppHidden =  dzenIcon "has_win.xbm" . \wsId -> dropIx wsId 
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs then "" else dzenColor mySeperatorColor myDzenBGColor . dzenIcon "has_win_nv.xbm" . dropIx $ wsId
    , ppUrgent = dzenColor myUrgentFGColor myUrgentBGColor . dzenIcon "has_win.xbm" . dzenStrip . \wsId -> dropIx wsId
    , ppSep = " "
    , ppWsSep = " "
    , ppTitle = dzenColor ("" ++ myNormalFGColor ++ "") "" . wrap "< " " >"
    , ppLayout = dzenColor ("" ++ myNormalFGColor ++ "") "" .
        (\x -> case x of
        "Full" -> "[ ]"
        "ResizableTall" -> "[|]"
        "Mirror ResizableTall" -> "[-]"
        "Tabbed ResizableTall" -> "[=]"
        "Tabbed Mirror ResizableTall" -> "[:]"
        "Tabbed Simplest" -> "[T]"
        "ThreeCol" -> "[3]"
        "Tabbed ThreeCol" -> "[%]"
        "OneBig 0.75 0.75" -> "[B]"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
            dropIx wsId = if (':' `elem` wsId) then drop 2 wsId else wsId
            dzenIcon iconName outputText = "^i(" ++ myIconDir ++ "/" ++ iconName ++ ")" ++ outputText
            staticWs = ["1:im", "2:local", "7:web", "8:dev", "9:remote"]
