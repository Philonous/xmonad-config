{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Applicative
import           Control.Arrow (second)
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Data.IORef
import qualified Data.List as List
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import Prelude hiding(log)
import           System.Directory
import           System.Exit
import           System.IO
import qualified System.Posix as Posix
import           System.Posix.Env (setEnv, unsetEnv)
import           System.Posix.Types
import           XMonad
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.FloatKeys
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.IM
import           XMonad.Layout.MouseResizableTile
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.Tabbed
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad

--import Keys

myShell = "/bin/zsh"
myTerminal      = "urxvt -e tmux new"
myTerminal2     = "xterm"

myBorderWidth   = 2
myModMask       = mod4Mask
myNumlockMask   = mod2Mask

myWorkspaces    = show <$> [1..22]

myNormalBorderColor  = "#300080"
myFocusedBorderColor = "#FF9000"

setVolume v = spawn $ "pactl set-sink-volume 1 " ++ v ++"%"

basicKeys scratchpads log conf =
  [ ("M-S-<Return>",  spawn $ XMonad.terminal conf)
  -- Volume control
 , ("M-<KP_Add>"        , setVolume "+5" )
 , ("M-<KP_Subtract>"   , setVolume "-5" )
 , ("M-S-<KP_Add>"      , setVolume "90" )
 , ("M-S-<KP_Subtract>" , setVolume "40" )
  -- close focused window
  , ("M-S-c", kill)
  -- Rotate through the available layout algorithms
--  , ("M-<Space>", sendMessage NextLayout)
  --  Reset the layouts on the current workspace to default
  , ("M-S-<Space>", setLayout $ XMonad.layoutHook conf)
  -- Resize viewed windows to the correct size
  , ("M-n", refresh)
  -- Move focus to the next window
  , ("M-<Tab>", windows W.focusDown)
  , ("M-S-<Tab>", windows W.focusUp)
  -- Move focus to the next window
  , ("M-k", windows W.focusDown)
  -- Move focus to the previous window
  , ("M-j", windows W.focusUp)
  -- Move focus to the master window
--   , ("M-m", windows W.focusMaster  )
  -- Swap the focused window and the master window
  , ("M-<Return>", windows W.swapMaster)
  -- Swap the focused window with the next window
  , ("M-S-j", windows W.swapDown  )
  -- Swap the focused window with the previous window
  , ("M-S-k", windows W.swapUp    )
  -- Shrink the master area
  , ("M-h", sendMessage Shrink)
  -- Expand the master area
  , ("M-l", sendMessage Expand)
  -- Push window back into tiling
  , ("M-t", withFocused $ windows . W.sink)
  -- Increment the number of windows in the master area
  , ("M-,", sendMessage (IncMasterN 1))
  -- Deincrement the number of windows in the master area
  , ("M-.", sendMessage (IncMasterN (-1)))
  -- Quit xmonad
  , ("M-S-q", spawn "xkill")
  -- Restart xmonad
  , ("M-q" , do
        recompiled <- recompile True
        when recompiled $ do
          broadcastMessage ReleaseResources
          restart "xmonad" True)
  , ("M-S-l", spawn "xscreensaver-command -lock" )
  , ("M-`", exclusivePad scratchpads "kuake")
  , ("M-r", withNamedPad scratchpads "run program" queryScratchpad)
  , ("M-g", withNamedPad scratchpads "ghci" queryScratchpad)
  , ("M--", withNamedPad scratchpads "hamster" queryScratchpad)
  , ("M-=", withNamedPad scratchpads "gtg" queryScratchpad)
  , ("M-<Insert>", withNamedPad scratchpads "gnumeric" queryScratchpad)
  , ("M-S-`", resetScratchpadWindow scratchpads)
  , ("M-<Pause>", spawn "hamster stop")



  , ("M-p", shellPrompt defaultXPConfig)
--  , ("M-m", return ())
  , ("C-`", return ())
  ]
  ++ [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
         | (key, scr)  <- zip "ew" [1,0] -- was [0..] *** change to match your screen order ***
         , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
    ]



moveToSide :: Side -> X ()
moveToSide = hookOnFocused . doSideFloat

modifyWindow :: (W.RationalRect -> W.RationalRect) -> X ()
modifyWindow = hookOnFocused . doFloatDep

hookOnFocused :: ManageHook -> X ()
hookOnFocused hook = withFocused (windows . appEndo <=< runQuery hook)

-- control + mod + key = start programme
programKeys log conf = allProgramsKey conf :
                       map (\(key,program) -> (("M-s " ++ key ), spawn program >> log ("spawning " ++ program)))
  [ ("d" , "dolphin"   )
  , ("p" , "pidgin"    )
  , ("e" , "emacs"     )
  , ("k" , "konqueror" )
  , ("c" , "xcalc"     )
  , ("x" , "xfce4-panel -r" )
  , ("X" , "emacs ~/.xmonad/xmonad.hs")
  ]
allProgramsKey conf = ("M-s a", delayedExecution programs ) where
  programs=
    [ ( "xfce4-panel" , 0 ) -- (Program to execute, delay afterwards)
    , ( "pidgin -n"   , 0 )
    , ( "hexchat"     , 0 )
    , ( "firefox"     , 0 )
    , ( "emacs"       , 0 )
    , ( "liferea"     , 0 )
    , ( "clementine"  , 0 )
    , ( "keepassx"    , 0 )
    , ( "evolution"   , 0 )
    , ( "owncloud"    , 0 )
    , ( "/usr/lib/kdeconnectd", 0)
    ]
  delayProgram :: Int -> String -> X ()
  delayProgram time prog = spawn $ "sleep " ++ show time ++ "; " ++ prog ++ " &"
  delayedExecution :: [(String, Int)] -> X ()
  delayedExecution (unzip -> (progs, delays)) =
    let delay = scanl (+) 0 delays in zipWithM_ delayProgram delay progs


    --
    -- mod-[1..9]++[F1..F12], Switch to workspace N
    -- mod-shift-[1..9]++[F1..F12], Move client to workspace N
    --
workspaceKeys log conf = M.fromList $ [((m .|. modMask conf, k), f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9]++[xK_0]++[xK_F1..xK_F12])
        , (f, m) <- [( \x -> windows (W.greedyView x) >> log ("WS: " ++ x), 0)
                     , (windows . W.shift, shiftMask)]]

windowKeys _ conf = M.fromList $
  [((modMask conf, key), withFocused $ keysMoveWindow direction ) |
    (key,direction) <-
      [ (xK_Left , (-step1,   0))
      , (xK_Right, ( step1,   0))
      , (xK_Up   , (  0, -step1))
      , (xK_Down , (  0,  step1))]]
  ++
  [((modMask conf .|. shiftMask, key), withFocused $ keysMoveWindow direction ) |
    (key,direction) <-
      [ (xK_Left , (-step2,    0))
      , (xK_Right, ( step2,    0))
      , (xK_Up   , (   0, -step2))
      , (xK_Down , (   0,  step2))]]
  ++
  [((modMask conf .|. controlMask , key), withFocused $ keysResizeWindow direction (0,0) ) |
    (key,direction) <-
      [ (xK_Left , (-step1,   0))
      , (xK_Right, ( step1,   0))
      , (xK_Up   , (  0, -step1))
      , (xK_Down , (  0,  step1))]]
  ++
  [((modMask conf .|. shiftMask .|. controlMask, key), withFocused $ keysResizeWindow direction (0,0) ) |
    (key,direction) <-
      [ (xK_Left , (-step2,    0))
      , (xK_Right, ( step2,    0))
      , (xK_Up   , (   0, -step2))
      , (xK_Down , (   0,  step2))]]
  ++ [((modMask conf, xK_space), sendMessage NextLayout)]
  -- mod + numpad moves window into respective corners
  ++ [((modMask conf, key), moveToSide side) |
       (key,side) <- zip
         [xK_KP_Home, xK_KP_Up   , xK_KP_Page_Up
         ,xK_KP_Left, xK_KP_Begin, xK_KP_Right
         ,xK_KP_End , xK_KP_Down , xK_KP_Page_Down
         ]
         [NW,NC,NE
         ,CW,C ,CE
         ,SW,SC,SE
         ]
     ]
  ++ [ ((modMask conf .|. controlMask, key), modifyWindowSize dy dx) |
       (key, (dx, dy)) <- zip
         [xK_KP_Home, xK_KP_Up   , xK_KP_Page_Up
         ,xK_KP_Left, xK_KP_Begin, xK_KP_Right
         ,xK_KP_End , xK_KP_Down , xK_KP_Page_Down
         ]
         (join (liftM2 (,) ) [1/2,1,2])
     ]
  ++ [ ((modMask conf .|. shiftMask, xK_KP_Begin), withFocused $ keysResizeWindow (myBorderWidth*2,myBorderWidth*2) (1/2, 1/2) )]
  where
    step1 = 32
    step2 = 256
    modifyWindowSize dw dh = modifyWindow
                       (\(W.RationalRect x y w h)
                       ->  W.RationalRect x y (w*dw) (h*dh))

--printKeys chan (masks,key) = io . writeChan chan $ concat (maskNames myModMask masks) ++ key

printingMap keys chan conf = mkKeymap conf $ keys conf
simpleMap keys conf = mkKeymap  conf $ keys conf

programKeys' = simpleMap . programKeys
basicKeys' scratchpad = simpleMap . basicKeys scratchpads

myKeys scratchpads log conf = foldr M.union M.empty $
              [programKeys', basicKeys' scratchpads, workspaceKeys, windowKeys]
                <*> [log]
                <*> [conf]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

tabbedTheme = defaultTheme{ activeColor = "#002b36"
                          , inactiveColor = "#93a1a1"
                          , urgentColor = "#93a1a1"
                          , activeTextColor = "#FF9000"
                          , inactiveTextColor = "#002b36"
                          , urgentTextColor = "#000000"
                          }

myLayout = onWorkspace "18" (withIM (1/5) (Role "buddy_list") (Mirror mouseResizableTile{draggerType = BordersDragger}) ) $
               tabbed shrinkText tabbedTheme |||
               mouseResizableTile {draggerType = BordersDragger } |||
               Mirror (mouseResizableTile {draggerType = BordersDragger })
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 2/3
     delta   = 3/100

ignoreNames =
  [ "desktop_window"
  , "kdesktop"
  , "Steam.exe"
  ]

floatNames =
  [ "MPlayer"
  , "Gimp"
  , "Yakuake"
  , "Xmessage"
  , "Xfce4-weather-plugin"
  , "Deskbar-applet"
  , "Plasma-desktop"
  , "Wine"
  ]

centerFloatNames =
  [ "Tomboy"
  , "Gnote"
  , "<interactive>"
  ]

setFullScreen = do
    w <- ask
    Query (lift . withDisplay $ \d -> liftIO $ setWindowBorderWidth d w 0)
    doFullFloat


myManageHook = composeAll . concat  $
               [concat $ uncurry byName <$>
                 [ (doIgnore     , ignoreNames     )
                 , (doFloat      , floatNames      )
                 , (doCenterFloat, centerFloatNames)
                 ]
               , [ isFullscreen --> doFullFloat
                 , isDialog     --> doFloat
                 , liftM2 (&&) isDialog (appName =? "Steam.exe")
                     --> doIgnore
                 , appName =? "dwarffortress" --> doIgnore
                 , className =? "hl2_linux" --> setFullScreen
                 , className =? "dota_linux" -->
                      (do doShift (workspace 19)
                          setFullScreen)

                 ]
               , map (\(cn, ws) -> className =? cn --> doShift (workspace ws) )
                 [ ( "Firefox"    , 1)
                 , ( "Namoroka"   , 1)
                 , ( "Xchat"      , 18)
                 , ( "Hexchat"    , 18)
                 , ( "Pidgin"     , 18)
                 , ( "Exail.py"   , 3)
                 , ( "Amarok"     , 3)
                 , ( "Clementine" , 3)
                 , ( "Emacs"      , 5)
                 , ( "Okular"     , 6)
                 , ( "Evince"     , 6)
                 , ( "Deluge"     , 7)
                 , ( "Nicotine"   , 7)
                 , ( "Ktorrent"   , 7)
                 , ( "Wine"       , 8)
                 , ("DwarfFortress", 8)
                 , ( "Claws-mail" , 10)
                 , ( "Evolution"  , 10)
                 , ( "Kmail"      , 10)
                 , ( "Keepassx"   , 11)
                 , ( "Liferea"    , 12)
                 , ( "Skype"      , 14)
                 , ( "Steam"      , 19)
                 ]
               , [ title =? "JDownloader" --> doShift (workspace 7) ]
               ]
  where workspace n = myWorkspaces !! (n-1)
        byName doX names = (\name -> className =? name --> doX ) <$> names

myFocusFollowsMouse = False

myLogHook = ewmhDesktopsLogHook
            >> setWMName "LG3D" -- solves the empty java windows issue

-- Send WM_TAKE_FOCUS
takeTopFocus = withWindowSet $ maybe (setFocusX =<< asks theRoot) takeFocusX . W.peek

-- atom_WM_TAKE_FOCUS      = getAtom "WM_TAKE_FOCUS"
takeFocusX w = withWindowSet $ \ws -> do
    dpy <- asks display
    wmtakef <- atom_WM_TAKE_FOCUS
    wmprot <- atom_WM_PROTOCOLS

    protocols <- io $ getWMProtocols dpy w
    when (wmtakef `elem` protocols) $ do
        io $ allocaXEvent $ \ev -> do
            setEventType ev clientMessage
            setClientMessageEvent ev w wmprot 32 wmtakef currentTime
            sendEvent dpy w False noEventMask ev

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM  p a b = p >>= \p -> if p then a else b

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM p a = ifM p a $ return ()

-- like ``liftM and . sequence'' but with shotcircuiting property
andM :: (Monad f, Functor f) => [f Bool] -> f Bool
andM [] = return True
andM (x:xs) = ifM x (andM xs) (return False)

main = do
  -- chan <- newChan
  -- forkIO $ pipeWriter "/home/uart14/.xmonad/pipemon.pipe" chan
  -- forkIO $ osdFun2
  spawn "reset-volume"
  spawn "keymap"
  xmonad . myConfig $ const (return ())

resetScratchpadWindow confs =
  forM_ confs $ \scratch ->
  withWindowSet $ \s -> do
    sPWindows <- filterM (runQuery $ appName =? name scratch) (W.allWindows s)
    wTrans <- forM sPWindows . runQuery $ hook scratch
    windows . appEndo . mconcat $ wTrans

inTerm name command = "urxvt -name " ++ name ++ " -e " ++ command

kuakeHook = doRectFloat $ W.RationalRect (1/16) 0 (1-2/16) (1-1/8)
cantoHook = doRectFloat $ W.RationalRect (1/16) (1/3) (1-2/16) (2/3)
krusaderHook = doRectFloat $ W.RationalRect (1/32) (0) (1-2/32) (31/32)
termKuake name command hook = NS { name  = name
                            , cmd   = inTerm name command
                            , query = appName =?  name
                            , hook  = hook
                            }

kuakeShellCmd kuakeTMuxSession command = concat ["sh -c '(tmux has-session -t " , kuakeTMuxSession
               , " && tmux -2 attach -t " , kuakeTMuxSession ,") "
               , "|| tmux -2 new-session -s " , kuakeTMuxSession,  " " , command, "'" ]


runProgramKuake = NS { name = "run program"
                     , cmd = "gmrun"
                     , query = appName =? "gmrun"
                     , hook = doCenterFloat
                     }

hamsterKuake = NS { name = "hamster"
                  , cmd = "zsh -c 'pkill -x hamster; hamster'"
                  , query = className =? "Hamster"
                  , hook = kuakeHook
                  }

gtgKuake =  NS { name = "gtg"
               , cmd = "gtg"
               , query = className =? "Gtg"
               , hook = kuakeHook
               }

gnumericPad = NS  { name = "gnumeric"
                  , cmd = "gnumeric"
                  , query = className =? "Gnumeric"
                  , hook = kuakeHook
                  }

scratchpads = [ termKuake "kuake" (kuakeShellCmd "SPad" myShell ) kuakeHook
              , termKuake "ghci"  (kuakeShellCmd "ghci" "ghci" ) kuakeHook
              , runProgramKuake
              , hamsterKuake
              , gtgKuake
              , gnumericPad
              ]


myConfig log = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
--        numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys scratchpads log,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = avoidStruts $ myLayout,
        manageHook         = manageDocks
                             <+> myManageHook
                             <+> namedScratchpadManageHook scratchpads,
        logHook            = myLogHook
    }


-------------------------------------------------------------------
-- helper stuff

scratchpadWorkspaceTag = "NSP"

ensureScratchpadWorkspaceExists s =
    if null (filter ((== scratchpadWorkspaceTag) . W.tag) (W.workspaces s))
    then addHiddenWorkspace scratchpadWorkspaceTag
    else return ()


banishScratchpads pads = withWindowSet $ \s -> do
    let currentWindows = maybe [] W.integrate . W.stack . W.workspace . W.current $ s
    banished <- forM pads $ \p -> do
        foundSPs <-  filterM (runQuery (query p)) currentWindows
        case foundSPs of
            [] -> return False
            _ -> do
                ensureScratchpadWorkspaceExists s
                windows . appEndo $ mconcat (Endo . W.shiftWin scratchpadWorkspaceTag <$> foundSPs)
                return True
    return $ or banished

banishScratchpad pad = withWindowSet $ \s -> do
    let currentWindows = maybe [] W.integrate . W.stack . W.workspace . W.current $ s
    foundSPs <-  filterM (runQuery $ query pad) currentWindows
    case foundSPs of
        [] -> return False
        _ -> do
            ensureScratchpadWorkspaceExists s
            windows . appEndo $ mconcat (Endo . W.shiftWin scratchpadWorkspaceTag <$> foundSPs)
            return True

queryScratchpad pad = withWindowSet $ \s -> do
    banished <- banishScratchpad pad
    case banished of
        True -> return False
        False -> do
            foundSPs <- filterM (runQuery $ query pad) (W.allWindows s)
            case null foundSPs of
                False -> windows . appEndo $ mconcat (Endo . W.shiftWin (W.currentTag s) <$> foundSPs)
                True -> spawn $ cmd pad
            return True

exclusivePad pads n = do
    banished <- banishScratchpads pads
    case banished of
        True -> return ()
        False -> withNamedPad pads n queryScratchpad



withNamedPad pads n action =
    forM_ (List.filter ((n ==) . name) pads) action
