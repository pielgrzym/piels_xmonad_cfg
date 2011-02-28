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
import XMonad.Hooks.ManageHelpers ( isFullscreen, isDialog, doCenterFloat, doFullFloat )
import XMonad.Layout.NoBorders
import XMonad.Actions.CycleWS
-- copy windows! tag-like functionality
import XMonad.Actions.CopyWindow
-- extra layouts
import XMonad.Layout.PerWorkspace 
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
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

import qualified Data.Map as M
import XMonad.Actions.TopicSpace
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Actions.FloatKeys
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
                , logHook            = dynamicLogWithPP $ myXmobarPP bar
                }
                `removeKeysP` [ "M-w", "M-e", "M-b" ] 
                `additionalKeysP`
                (
                [ ("M-r",       spawn (myDmenu))
                , ("M-g",       goToSelected $ gsconfig2 greenColorizer) -- window grid
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
   , "email"
   , "proj", "debug"
   , "doc", "music", "web"
   , "admin"
   , "im"
   -- >9 topics:
   , "vbox"
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
    , defaultTopicAction = const (return ())
    --, defaultTopicAction = const $ spawnShell
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
        , ("music",     spawn "xterm -e cmus")
        , ("gothic",    spawnShell)
        , ("doc",       spawnShell >>
                        spawnShellIn "doc")
        , ("vbox",      spawn "VirtualBox")
        , ("gimp",      spawn "gimp")
        , ("email",     spawn "jumanji poczta.prymityw.pl")
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
myFont = "snap"
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

gsconfig2 colorizer = (buildDefaultGSConfig colorizer) { gs_cellheight = 30, gs_cellwidth = 100 }
greenColorizer = colorRangeFromClassName
                      black            -- lowest inactive bg
                      green            -- highest inactive bg
                      yellow           -- active bg
                      white            -- inactive fg
                      black            -- active fg
   where black = minBound
         white = maxBound
         yellow = (0xFF,0xFF,0x70)
         green = (0x00,0xFF,0x00)


-- layout hook
myLayout = avoidStruts 
        $ smartBorders
        $ configurableNavigation noNavigateBorders
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
    , className =? "Skype"          --> doShift "im"
    , className =? "Pidgin"         --> doShift "im"
    , resource  =? "desktop_window" --> doIgnore
    ]

myStatusBar = "xmobar -x 1"
 
myXmobarPP h = defaultPP
    { ppCurrent = wrap ("[<fc=" ++ myUrgentFGColor ++ ">") "</fc>]" . \wsId -> dropIx wsId
    , ppVisible = wrap ("[<fc=" ++ myNormalFGColor ++ ">") "</fc>]" . \wsId -> dropIx wsId
    , ppHidden = wrap "" "" . \wsId -> dropIx wsId -- don't use <fc> here!!
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
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
    dropIx wsId = if (':' `elem` wsId) then drop 2 wsId else wsId
    staticWs = ["start", "web", "proj", "@", "admin"]
