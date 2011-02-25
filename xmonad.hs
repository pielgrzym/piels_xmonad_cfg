import XMonad hiding ( (|||) )
import XMonad.Layout.LayoutCombinators
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
import XMonad.Layout.OneBig
import XMonad.Layout.Magnifier
import XMonad.Layout.Maximize
import XMonad.Layout.Circle
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

import qualified Data.Map as M
import XMonad.Actions.TopicSpace
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Actions.FloatKeys
-- todo:
-- * xmobar
-- * prompt - szatki
-- * grid select - szatki
-- * lepsze pomysły na topic spaces
-- * nowe layouty dla ts
main= do 
        bar <- spawnPipe myStatusBar
        spawn "unclutter -idle 3"
        spawn "syndaemon -k -d -i 2 -t"
        --urxvtd <- spawnPipe "urxvtd -q -f"
        checkTopicConfig myTopics myTopicConfig
        xmonad $ withUrgencyHook NoUrgencyHook
               $ defaultConfig 
                { 
                borderWidth          = 3
                , terminal           = "xterm"
                --, terminal           = "urxvtc"
                , normalBorderColor  = "#262626"
                , focusedBorderColor = "green" 
                , modMask            = mod4Mask     -- Rebind Mod to the Windows key 
                , workspaces         = myWorkspaces
                , manageHook         = myManageHook <+> manageDocks -- <+> manageMonitor clock
                , layoutHook         = myLayout
                --, logHook            = dynamicLogWithPP $ myDzenPP bar
                , logHook            = dynamicLogWithPP $ myXmobarPP bar
                }
                --`removeKeysP` ["M-" ++ [n] | n <- ['1'..'9']]
                --`removeKeysP` ["M-S-" ++ [n] | n <- ['1'..'9']]
                --`removeKeysP` ["M-C-" ++ [n] | n <- ['1'..'9']]
                `removeKeysP` [ "M-w", "M-e", "M-b" ] 
                `additionalKeysP`
                (
                [ ("M-r",       spawn (myDmenu))
                , ("M-g",       goToSelected defaultGSConfig) -- window grid
                --, ("M-n",     sendMessage MirrorShrink)
                , ("M-n",       nextWS)
                , ("M-p",       prevWS)
                , ("M-u",       focusUrgent)
                , ("M-f",       withFocused (sendMessage . maximizeRestore))
                -- cmus control
                , ("M-z",       spawn "cmus-remote --prev")
                , ("M-x",       spawn "cmus-remote --play")
                , ("M-c",       spawn "cmus-remote --pause")
                , ("M-v",       spawn "cmus-remote --stop")
                , ("M--",       spawn "cmus-remote --vol -10%")
                , ("M-=",       spawn "cmus-remote --vol +10%")
                -- eof cmus control
                , ("M-<F8>",    sendMessage $ JumpToLayout "Circle")
                , ("M-<F9>",    sendMessage $ JumpToLayout "Tabbed Simplest")
                , ("M-<F10>",   sendMessage $ JumpToLayout "Tabbed Spacing 2 ResizableTall")
                , ("M-<F11>",   sendMessage $ JumpToLayout "Magnifier Spacing 2 ResizableTall")
                , ("M-<F12>",   sendMessage $ JumpToLayout "Magnifier Mirror Spacing 2 ResizableTall")
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
                -- topic space related keybindings
                , ("M-;",       promptedGoto) -- TS goto
                , ("M-S-;",     promptedShift) -- TS shift
                , ("M-'",       toggleWS) -- switch to previous topic
                -- window nav
                , ("C-M-l",       sendMessage $ Go R)
                , ("C-M-h",       sendMessage $ Go L)
                , ("C-M-j",       sendMessage $ Go D)
                , ("C-M-k",       sendMessage $ Go U)
                -- float
                , ("M-<L>", withFocused (keysMoveWindow (-20,0))) -- move float left
                , ("M-<R>", withFocused (keysMoveWindow (20,0))) -- move float right
                , ("M-<U>", withFocused (keysMoveWindow (0,-20))) -- move float up
                , ("M-<D>", withFocused (keysMoveWindow (0,20))) -- move float down
                , ("M-S-<L>", withFocused (keysResizeWindow (-20,0) (0,0))) --shrink float at right
                , ("M-S-<R>", withFocused (keysResizeWindow (20,0) (0,0))) --expand float at right
                , ("M-S-<D>", withFocused (keysResizeWindow (0,20) (0,0))) --expand float at bottom
                , ("M-S-<U>", withFocused (keysResizeWindow (0,-20) (0,0))) --shrink float at bottom
                , ("M-C-<L>", withFocused (keysResizeWindow (20,0) (1,0))) --expand float at left
                , ("M-C-<R>", withFocused (keysResizeWindow (-20,0) (1,0))) --shrink float at left
                , ("M-C-<U>", withFocused (keysResizeWindow (0,20) (0,1))) --expand float at top
                , ("M-C-<D>", withFocused (keysResizeWindow (0,-20) (0,1))) --shrink float at top
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
                        , (action, m) <- [(switchTopic myTopicConfig, ""), (windows . W.shift, "S-"), (windows . copy, "C-")]]
                )

myWorkspaces = myTopics

myTopics :: [Topic]
myTopics =
   [ "start" -- the first one
   , "@"
   , "proj", "debug"
   , "doc", "music", "web"
   , "admin"
   , "im"
   , "vbox"
   -- >9 topics:
   --, "euler", "newton", "fermat", "rzedzian"
   , "1", "2", "3", "4" -- general purpose topics
   , "xmonad"
   , "mov"
   , "gimp"
   , "gothic"
   ]

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
    { topicDirs = M.fromList $
        [ ("start", "~")
        , ("proj", "proj")
        , ("debug", "proj")
        , ("xmonad", "~/.xmonad")
        , ("mov", "mov")
        , ("music", "muza")
        , ("doc", "Dropbox")
        , ("gothic", "~/gothic/g")
        ]
    , defaultTopicAction = const $ spawnShell
    , defaultTopic = "net"
    , maxTopicHistory = 10
    , topicActions = M.fromList $
        [ ("start",     spawnShell)
        , ("xmonad",    spawnShellIn ".xmonad" >>
                        spawnShellIn ".xmonad")
        , ("web",       spawn "opera")
        , ("admin",     spawnShell >*> 3 >>
                        spawn "jumanji 172.29.0.1:8080")
        , ("mov",       spawnShell)
        , ("gothic",    spawnShell)
        , ("doc",       spawnShell >>
                        spawnShellIn "doc")
        , ("vbox",      spawn "VirtualBox")
        , ("gimp",      spawn "gimp")
        , ("@",         spawn "jumanji poczta.prymityw.pl")
        ]
    }


myShell = "zsh"
spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn
spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "xterm -e \"cd " ++ dir ++ "; " ++ myShell ++ "\""
goto :: Topic -> X ()
goto = switchTopic myTopicConfig
promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto
promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift

myDmenu = "dmenu_run -fn terminus -nf \""++myDzenFGColor++"\" -nb \""++myDzenBGColor++"\" -sb \""++myDzenFGColor++"\" -sf \""++myDzenBGColor++"\""

myXPConfig = defaultXPConfig {
        font = myFont
        , fgColor = "green"
        , bgColor = "#262626"
        , promptBorderWidth = 0
        , fgHLight = "black"
        , bgHLight = "green"
        , autoComplete = Just 1000
}

-- Color, font and iconpath definitions:
--myFont = "-xos4-terminus-medium-r-normal-*-14-*-*-*-c-*-iso10646-1"
myFont = "snap"
--myFont = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u"
myIconDir = "/home/pielgrzym/.xmonad/icons"
myDzenFGColor = "green"
myDzenBGColor = "#262626"
myNormalFGColor = "#ffffff"
myNormalBGColor = "#0f0f0f"
myFocusedFGColor = "#f0f0f0"
myFocusedBGColor = "#333333"
myUrgentFGColor = "#00ff00"
myUrgentBGColor = "#0077ff"
myIconFGColor = "#777777"
myIconBGColor = "#0f0f0f"
mySeperatorColor = "#555555"


-- layout hook
myLayout = avoidStruts 
        $ smartBorders
        $ windowNavigation
        $ maximize
        $ boringWindows
        $ onWorkspace "im" (enableTabs three_col')
        $ onWorkspace "web" big_layouts
        $ onWorkspace "gothic" big_layouts
        $ default_layouts
        where
            default_layouts = (tabbed' ||| enableTabs resizable_tall' ||| enableTabs (Mirror resizable_tall') ||| magni_tall ||| mirror_magni_tall ||| Circle)
            big_layouts = (tabbed' ||| Full ||| magni_tall)
            -- complex layout definitions:
            resizable_tall' = spacing 2 $ ResizableTall 1 (3/100) (1/2) []
            tabbed'        = withBorder 1 $ tabbed shrinkText myTabTheme
            three_col'     = spacing 2 $ ThreeColMid 2 (3/100) (4/5)
            enableTabs x  = addTabs shrinkText myTabTheme $ subLayout [] Simplest x
            magni_tall = magnifier resizable_tall'
            mirror_magni_tall = magnifier (Mirror resizable_tall')
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
    , fontName = "snap"
    --, decoWidth = ""
    , decoHeight = 16
    }

-- manage hook
myManageHook = composeAll
    [ isFullscreen                  --> doFullFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "Smplayer"       --> doFloat
    , className =? "feh"            --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Conky"          --> doIgnore
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

--myStatusBar = "dzen2 -x '0' -y '0' -h '14' -w '600'  -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myDzenBGColor ++ "' -fn '" ++ myFont ++ "'"
myStatusBar = "xmobar -x 1"
 
myXmobarPP h = defaultPP
    { ppCurrent = wrap ("[<fc=" ++ myUrgentFGColor ++ ">") "</fc>]" . \wsId -> dropIx wsId
    , ppVisible = wrap ("[<fc=" ++ myNormalFGColor ++ ">") "</fc>]" . \wsId -> dropIx wsId
    , ppHidden = wrap "" "" . \wsId -> dropIx wsId -- don't use <fc> here!!
    --, ppHiddenNoWindows = wrap ("<fc=" ++ myDzenFGColor ++ ">") "</fc>" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs then "" else wrap ("<fc=" ++ mySeperatorColor ++ ">") "</fc>" . dropIx $ wsId
    , ppUrgent = wrap ("<fc=" ++ myUrgentFGColor ++ ">!") "!</fc>" . \wsId -> dropIx wsId
    , ppSep = " "
    , ppWsSep = " "
    , ppTitle = xmobarColor (""++ myIconFGColor ++ "") "" . wrap "[ " " ]"
    , ppLayout = xmobarColor ("red") "" .
        (\x -> case x of
        "Full" -> "[ ]"
        "Maximize Tabbed Spacing 2 ResizableTall" -> "[|]"
        "Maximize Magnifier Spacing 2 ResizableTall" -> "[:]"
        "Maximize Tabbed Mirror Spacing 2 ResizableTall" -> "[-]"
        "Maximize Magnifier Mirror Spacing 2 ResizableTall" -> "[=]"
        "Maximize Tabbed Simplest" -> "[T]"
        "Maximize Tabbed Spacing 2 ThreeCol" -> "[3]"
        "Maximize Circle" -> "[O]"
        "OneBig 0.75 0.75" -> "[B]"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
    dropIx wsId = if (':' `elem` wsId) then drop 2 wsId else wsId
    staticWs = ["start", "web", "proj", "@", "admin"]

myDzenPP h = defaultPP
    { ppCurrent = wrap ("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()^i(" ++ myIconDir ++ "/full.xbm) ^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppVisible = wrap ("^fg(" ++ myNormalFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p()^i(" ++ myIconDir ++ "/empty.xbm) ^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppHidden = wrap ("^i(" ++ myIconDir ++ "/empty.xbm) ") "^fg()^bg()^p()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId -- don't use ^fg() here!!
    --, ppHiddenNoWindows = wrap ("^fg(" ++ myDzenFGColor ++ ")^bg()^p()^i(" ++ myIconDir ++ "/corner.xbm)") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs then "" else wrap ("^fg(" ++ mySeperatorColor ++ ")^bg()^p()^i(" ++ myIconDir ++ "/empty.xbm) ") "^fg()^bg()^p()" . dropIx $ wsId
    , ppUrgent = wrap (("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p()^i(" ++ myIconDir ++ "/bug_01.xbm) ^fg(" ++ myUrgentFGColor ++ ")")) "^fg()^bg()^p()" . \wsId -> dropIx wsId
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
            staticWs = ["net", "im", "web", "admin"]
