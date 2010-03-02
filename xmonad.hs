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
-- extra layouts
import XMonad.Layout.PerWorkspace 
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.OneBig
-- sublayouts
import XMonad.Layout.SubLayouts(GroupMsg(UnMergeAll, UnMerge, MergeAll), defaultSublMap, onGroup, pullGroup, pushWindow, subLayout, subTabbed)
import XMonad.Layout.WindowNavigation
import XMonad.Layout.BoringWindows
import XMonad.Layout.Simplest
-- urgency
import XMonad.Hooks.UrgencyHook

main= do 
        bar <- spawnPipe myStatusBar
        xmonad $ withUrgencyHook NoUrgencyHook
               $ defaultConfig 
                { 
                borderWidth          = 1
                , terminal           = "urxvt"
                , normalBorderColor  = "#262626"
                , focusedBorderColor = "#ff5f00" 
                , modMask            = mod4Mask     -- Rebind Mod to the Windows key 
                , workspaces         = myWorkspaces
                , manageHook         = myManageHook <+> manageDocks
                , layoutHook         = myLayout
                --, logHook            = dynamicLogWithPP $ myDzenPP bar
                , logHook            = dynamicLogWithPP $ myXmobarPP bar
                }
                `additionalKeysP`
                (
                [ ("M-r", spawn "dmenu_run")
                , ("M-g", goToSelected defaultGSConfig)
                , ("M-n", sendMessage MirrorShrink)
                , ("M-b", sendMessage MirrorExpand)
                , ("M-u",  focusUrgent)
                , ("M-M1-h",    sendMessage Shrink)                     -- Resize Window
                , ("M-M1-l",   sendMessage Expand)
                , ("M-M1-k",              sendMessage MirrorExpand)
                , ("M-M1-j",    sendMessage MirrorShrink)
                , ("M-m h",             sendMessage $ pullGroup L)      -- Merge to Tabbed
                , ("M-m l",    sendMessage $ pullGroup R)
                , ("M-m k",               sendMessage $ pullGroup U)
                , ("M-m j",             sendMessage $ pullGroup D)
                , ("M-m m",                  withFocused (sendMessage . MergeAll))
                , ("M-m S-m",                withFocused (sendMessage . UnMergeAll))
                , ("M-S-m",                  withFocused (sendMessage . UnMerge))
                , ("M-S-,",                    onGroup W.focusUp')                     -- Move focus between tabs
                , ("M-S-.",                    onGroup W.focusDown')           -- Move focus between tabs
                ]
                ++
                [("M-"++m++[key], screenWorkspace sc >>= flip whenJust (windows . f))
                        | (f, m) <- [(W.view, ""), (W.shift, "S-")]
                        , (key, sc) <- zip "io" [0 .. ]]
                )
                `removeKeysP` [ "M-w", "M-e", "M-p" ]

myWorkspaces = ["1:im", "2:www", "3:dev", "4:music", "5:misc", "6:gimp", "7:mplayer", "8:fs", "9:vbox"]
-- Color, font and iconpath definitions:
--myFont = "-xos4-terminus-medium-r-normal-*-14-*-*-*-c-*-iso10646-1"
myFont = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u"
myIconDir = "/home/pielgrzym/.dzen"
myDzenFGColor = "#ff5f00"
myDzenBGColor = "#262626"
myNormalFGColor = "#ffffff"
myNormalBGColor = "#0f0f0f"
myFocusedFGColor = "#f0f0f0"
myFocusedBGColor = "#333333"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#0077ff"
myIconFGColor = "#777777"
myIconBGColor = "#0f0f0f"
mySeperatorColor = "#555555"


-- layout hook
myLayout = avoidStruts 
        $ windowNavigation
        $ boringWindows
        $ onWorkspace "1:im" (three_col' ||| enableTabs three_col')
        $ onWorkspaces ["2:www", "9:vbox"] big_layouts
        $ onWorkspace "3:dev" (tabbed' ||| resizable_tall')
        $ onWorkspaces ["4:music", "8:fs"] small_layouts
        $ all_layouts
        where
            all_layouts = (resizable_tall' ||| Mirror resizable_tall' ||| Full ||| tabbed' ||| three_col' ||| onebig')
            big_layouts = (Full ||| tabbed' ||| onebig' ||| resizable_tall' ||| Mirror resizable_tall' )
            small_layouts = (onebig' ||| resizable_tall' ||| Mirror resizable_tall')
            -- default tiling algorithm partitions the screen into two panes
            resizable_tall' = ResizableTall 1 (3/100) (1/2) [] 
            onebig'        = OneBig (3/4) (3/4)
            tabbed'        = tabbed shrinkText myTabTheme
            three_col'     = ThreeColMid 2 (3/100) (2/3)
            enableTabs x  = addTabs shrinkText myTabTheme $ subLayout [] Simplest x

         
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
    , decoHeight = 14
    }

-- manage hook
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "SMPlayer"       --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

myStatusBar = "xmobar"
 
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
    staticWs = ["1:im", "2:www", "3:dev", "4:music", "5:misc"]
