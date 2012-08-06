import XMonad hiding ( (|||) )
import XMonad.Layout.LayoutCombinators
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers(doRectFloat, Side (C))
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP, removeKeysP)
import System.IO
import qualified XMonad.StackSet as W
import XMonad.Actions.GridSelect
import XMonad.Util.Loggers
import XMonad.Hooks.ManageHelpers ( isFullscreen, isDialog, doCenterFloat, doFullFloat )
import XMonad.Layout.NoBorders
import XMonad.Actions.CycleWS
import XMonad.Util.Scratchpad
-- copy windows! tag-like functionality
import XMonad.Actions.CopyWindow
import XMonad.Actions.DynamicWorkspaces
-- extra layouts
import XMonad.Layout.PerWorkspace 
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Maximize
import XMonad.Layout.Named
import XMonad.Layout.DecorationMadness
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
import XMonad.Prompt.Input
import XMonad.Actions.FloatKeys

import XMonad.Actions.UpdatePointer

main= do 
        bar <- spawnPipe myStatusBar
        --spawn "xmobar ~/.xmonad/xmobarrc2 -x 1"
        --spawn "unclutter -idle 3"
        --spawn "nitrogen --restore"
        --urxvtd <- spawnPipe "urxvtd -q -f"
        checkTopicConfig myTopics myTopicConfig
        xmonad $ withUrgencyHook NoUrgencyHook
               $ defaultConfig 
                { 
                borderWidth          = 3
                , terminal           = "urxvtc"
                , normalBorderColor  = "#262218"
                , focusedBorderColor = "#ed3e52"
                , modMask            = mod4Mask     -- Rebind Mod to the Windows key 
                , workspaces         = myWorkspaces
                , manageHook         = myManageHook <+> manageDocks <+> manageScratchPad
                , layoutHook         = avoidStruts $ myLayout
                , logHook            = (dynamicLogWithPP $ myXmobarPP bar) >> updatePointer (Relative 0.5 0.5)
                }
                `removeKeysP` [ "M-w", "M-e", "M-b" ] 
                `additionalKeysP`
                (
                [ ("M-r",       spawn (myDmenu))
                , ("M-g",       goToSelected $ gsconfig2 greenColorizer) -- window grid
                , ("M-j",       focusDown)
                , ("M-<Return>", spawn("urxvtc"))
                , ("M-S-<Return>", windows W.swapMaster)
                , ("M-k",       focusUp)
                , ("M-u",       focusUrgent)
                , ("M-p",       scratchPad)
                -- , ("M-c",       inboxPrompt)
                , ("M-f",       withFocused (sendMessage . maximizeRestore))
                -- audio controls
                , ("<XF86Launch1>",           spawn "~/.xmonad/toggle_display.sh")
                , ("<XF86AudioMute>",           spawn "amixer -c 0 -- sset Master toggle && echo \"Mute!\" | dzen2 -xs 1 -p 1 -bg yellow -fg black -fn '-*-terminus-*-*-*-*-12-*-*-*-*-*-*'")
                , ("<XF86AudioLowerVolume>",    spawn "amixer -c 0 -- sset Master 5%- && echo \"Sound down!\" | dzen2 -xs 1 -p 1 -bg yellow -fg black -fn '-*-terminus-*-*-*-*-12-*-*-*-*-*-*'")
                , ("<XF86AudioRaiseVolume>",    spawn "amixer -c 0 -- sset Master 5%+&& echo \"Sound up!\" | dzen2 -xs 1 -p 1 -bg yellow -fg black -fn '-*-terminus-*-*-*-*-12-*-*-*-*-*-*'")
                -- screen lock
                , ("<XF86ScreenSaver>",    spawn "slock")
                -- eof cmus control
                -- open a project - spawn gvim and 2 urxvt windows
                , ("M-<F2>",    spawn "popen")
                , ("M-<F8>",    sendMessage $ JumpToLayout "[T]")
                , ("M-<F9>",    sendMessage $ JumpToLayout "[|]")
                , ("M-<F10>",   sendMessage $ JumpToLayout "[-]")
                , ("M-<F11>",   sendMessage $ JumpToLayout "[:]")
                , ("M-<F12>",   sendMessage $ JumpToLayout "[=]")
                , ("M-S-c",     kill1)  -- remove a window copy or kill window otherwise
                , ("M-M1-k",    sendMessage MirrorExpand)
                , ("M-M1-j",    sendMessage MirrorShrink)
                , ("M-n",       sendMessage $ ToggleStrut R) -- Merge to Tabbed
                , ("M-m M-h",   sendMessage $ pullGroup L) -- Merge to Tabbed
                , ("M-m M-l",   sendMessage $ pullGroup R)
                , ("M-m M-k",   sendMessage $ pullGroup U)
                , ("M-m M-j",   sendMessage $ pullGroup D)
                , ("M-m S-m",   withFocused (sendMessage . UnMergeAll))
                , ("M-S-m",     withFocused (sendMessage . UnMerge))
                , ("M-[",       onGroup W.focusUp') -- Move focus between tabs
                , ("M-]",       onGroup W.focusDown') -- Move focus between tabs
                -- topic space related keybindings
                , ("M-;",       promptedGoto) -- TS goto
                , ("M-S-;",     promptedShift) -- TS shift
                , ("M-C-;",     promptedCopy) -- TS shift
                , ("M-d",       promptNewWS) -- new workspace
                , ("M-S-<Backspace>",     removeWorkspace) -- remove workspace
                , ("M-'",       toggleWS) -- switch to previous topic
                -- window nav
                , ("C-M-l",     sendMessage $ Go R)
                , ("C-M-h",     sendMessage $ Go L)
                , ("C-M-j",     sendMessage $ Go D)
                , ("C-M-k",     sendMessage $ Go U)
                -- float
                , ("M-<L>",     withFocused (keysMoveWindow (-20,0))) -- move float left
                , ("M-<R>",     withFocused (keysMoveWindow (20,0))) -- move float right
                , ("M-<U>",     withFocused (keysMoveWindow (0,-20))) -- move float up
                , ("M-<D>",     withFocused (keysMoveWindow (0,20))) -- move float down
                , ("M-S-<L>",   withFocused (keysResizeWindow (-20,0) (0,0))) --shrink float at right
                , ("M-S-<R>",   withFocused (keysResizeWindow (20,0) (0,0))) --expand float at right
                , ("M-S-<D>",   withFocused (keysResizeWindow (0,20) (0,0))) --expand float at bottom
                , ("M-S-<U>",   withFocused (keysResizeWindow (0,-20) (0,0))) --shrink float at bottom
                , ("M-C-<L>",   withFocused (keysResizeWindow (20,0) (1,0))) --expand float at left
                , ("M-C-<R>",   withFocused (keysResizeWindow (-20,0) (1,0))) --shrink float at left
                , ("M-C-<U>",   withFocused (keysResizeWindow (0,20) (0,1))) --expand float at top
                , ("M-C-<D>",   withFocused (keysResizeWindow (0,-20) (0,1))) --shrink float at top
                ]
                ++
                -- below: screen swithing with 'i' and 'o'
                [("M-"++m++[key], screenWorkspace sc >>= flip whenJust (windows . f))
                        | (f, m) <- [(W.view, ""), (W.shift, "S-"), (copy, "C-")]
                        , (key, sc) <- zip "oi" [0 .. ]]
                ++
                -- below: workspace greedy switch with M-[0..9], move to ws with M-S-[0..9] and copy to ws with M-C-[0..9]
                [("M-"++m++[key], action tag)
                        | (tag, key) <- zip myWorkspaces ['1'..'9']
                        , (action, m) <- [(switchTopic myTopicConfig, ""), (windows . W.shift, "S-"), (windows . copy, "C-")]]
                )

myWorkspaces = myTopics

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
         green = (0x70,0xFF,0x70)

myTopics :: [Topic]
myTopics =
   [ "dashboard" -- the first one
   , "web"
   , "admin"
   , "im"
   , "vbox"
   , "cfg"
   , "films"
   , "gimp"
   ]

scratchPad = scratchpadSpawnActionTerminal "urxvtc"
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect 0.05 0.05 0.9 0.5)

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
    { topicDirs = M.fromList $
        [ ("dashboard", "~")
        , ("admin", "~/work")
        , ("cfg", "~/.xmonad")
        , ("im", "~")
        , ("films", "~/download")
        ]
    , defaultTopicAction = const (return ())
    --, defaultTopicAction = const $ spawnShell
    , defaultTopic = "start"
    , maxTopicHistory = 10
    , topicActions = M.fromList $
        [ ("dashboard",     spawnShell >>
                        spawn "conky -c ~/.conky/conky_grey")
        , ("web",       spawn "firefox")
        , ("im",        spawnShell)
        , ("cfg",       spawnShell)
        , ("admin",     spawnShell)
        , ("films",     spawnShell)
        , ("vbox",      spawn "VirtualBox")
        , ("gimp",      spawn "gimp")
        ]
    }


myShell = "zsh"
spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn
spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "urxvtc -cd " ++ dir 
goto :: Topic -> X ()
goto = switchTopic myTopicConfig
promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto
promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift
promptedCopy :: X ()
promptedCopy = workspacePrompt myXPConfig $ windows . copy


myDmenu = "dmenu_run -fn terminus -nf \""++myDzenFGColor++"\" -nb \""++myDzenBGColor++"\" -sb \""++myDzenFGColor++"\" -sf \""++myDzenBGColor++"\""

myXPConfig = defaultXPConfig {
        font = myFont
        , fgColor = myUrgentFGColor
        , bgColor = myUrgentBGColor
        , promptBorderWidth = 0
        , fgHLight = "green"
        , bgHLight = "black"
        , autoComplete = Just 1000
}

-- Color, font and iconpath definitions:
myFont = "xft:snap:pixelsize=10"
myDzenFGColor = "#7d6f50"
myDzenBGColor = "#262218"
myNormalFGColor = "#7d6f50"
myNormalBGColor = "#262218"
myFocusedFGColor = "#ed3e52"
myFocusedBGColor = "#262218" 
myUrgentFGColor = "#262218"
myUrgentBGColor = "#ed813e"
myIconFGColor = "#777777"
mySeperatorColor = "#555555"

-- layout hook
myLayout = avoidStruts 
        $ smartBorders
        $ configurableNavigation noNavigateBorders
        $ boringWindows
        $ onWorkspace "gimp" (circle')
        $ default_layouts
        where
            default_layouts = (tabbed' ||| resizable_tall' ||| mirror_resizable_tall' ||| circle')
            -- default_layouts = (tabbed' ||| resizable_tall' ||| mirror_resizable_tall' ||| magni_tall ||| mirror_magni_tall)
            --big_layouts = (tabbed' ||| Full ||| magni_tall)
            -- complex layout definitions:
            circle' = named "[%]" $ circleDecoResizable shrinkText myTabTheme
            resizable_tall' = named "[|]" $ maximize $ enableTabs $ spacing 2 $ ResizableTall 1 (3/100) (1/2) []
            mirror_resizable_tall' = named "[-]" $ maximize $ enableTabs $ spacing 2 $ Mirror $ ResizableTall 1 (3/100) (4/5) []
            tabbed'        = named "[T]" $ withBorder 1 $ maximize $ tabbed shrinkText myTabTheme
            enableTabs x  = addTabs shrinkText myTabTheme $ subLayout [] Simplest x
         
-- tabbed theme
myTabTheme = defaultTheme
    { activeColor = "" ++ myFocusedFGColor ++ ""
    , activeTextColor = myFocusedBGColor
    , activeBorderColor = "" ++ myFocusedFGColor ++ ""
    , inactiveColor = "" ++ myNormalBGColor ++ ""
    , inactiveTextColor = "" ++ myNormalFGColor ++ ""
    , inactiveBorderColor ="" ++ myNormalBGColor ++ ""
    , urgentColor = "" ++ myUrgentBGColor ++ ""
    , urgentTextColor = "" ++ myUrgentFGColor ++ ""
    , urgentBorderColor = "" ++ myUrgentFGColor ++ ""
    , fontName = myFont
    , decoHeight = 15
    }

-- manage hook
myManageHook = composeAll
    [ isFullscreen                  --> doFullFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "Smplayer"       --> doFloat
    , className =? "Xmessage"       --> doCenterFloat
    , className =? "feh"            --> doFloat
    , className =? "Gimp"           --> doShift "gimp"
    -- , className =? "Conky"          --> doFloat
    , className =? "Clementine"     --> doShift "muza"
    , title     =? "Zapisz jako"    --> doRectFloat (W.RationalRect 0.05 0.05 0.6 0.6)
    , title     =? "Save As"        --> doRectFloat (W.RationalRect 0.05 0.05 0.6 0.6)
    , title     =? "Otwarcie obrazu"--> doRectFloat (W.RationalRect 0.05 0.05 0.6 0.6)
    , title     =? "Zapis obrazu"   --> doRectFloat (W.RationalRect 0.05 0.05 0.6 0.6)
    , title     =? "Save Image"     --> doRectFloat (W.RationalRect 0.05 0.05 0.6 0.6)
    , title     =? "Otwórz pliki"   --> doRectFloat (W.RationalRect 0.05 0.05 0.6 0.6)
    , title     =? "Enter name of file to save to…"   --> doRectFloat (W.RationalRect 0.05 0.05 0.6 0.6)
    , className =? "Skype"          --> doShift "im"
    , className =? "Pidgin"         --> doShift "im"
    , resource  =? "desktop_window" --> doIgnore
    ]

myStatusBar = "xmobar -x 0"
 
myXmobarPP h = defaultPP
    { ppCurrent = wrap ("[<fc="++ myFocusedFGColor ++">") "</fc>]"
    , ppVisible = wrap ("[<fc=" ++ myNormalFGColor ++ ">") "</fc>]"
    , ppHidden = wrap "" "" . \wsId -> noScratchPad wsId
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs then "" else wrap ("<fc=" ++ mySeperatorColor ++ ">") "</fc>" . noScratchPad $ wsId
    , ppUrgent = wrap ("<fc=" ++ myUrgentFGColor ++ ">!") "!</fc>"
    , ppSep = " "
    , ppWsSep = " "
    , ppTitle = xmobarColor (""++ myIconFGColor ++ "") "" . wrap "[ " " ]"
    , ppLayout = xmobarColor (myFocusedFGColor) "" 
    , ppOutput = hPutStrLn h
    }
    where
    noScratchPad ws = if ws == "NSP" then "" else ws
    staticWs = ["start", "web", "proj", "email", "admin"]

myXPInboxConfig = myXPConfig {
        font = "xft:Terminus:pixelsize=12"
        --font = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*"
}

-- inboxPrompt :: X()
-- inboxPrompt = inputPrompt myXPInboxConfig "INBOX" ?+ addToInbox
-- addToInbox :: String -> X()
-- addToInbox x = liftIO $ appendFile "/home/pielgrzym/otl/inbox.otl" ("[_] " ++ x ++ "\n")

promptNewWS :: X ()
promptNewWS = inputPrompt myXPInboxConfig "New WS" ?+ addWorkspace
