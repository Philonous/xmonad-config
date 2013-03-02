{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns #-}

import XMonad
import System.Exit

import Control.Monad
import Control.Concurrent
import Control.Arrow (second)


import XMonad.Actions.FloatKeys

import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.SimpleFloat
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.MouseResizableTile
import XMonad.Util.NamedScratchpad

import Control.Applicative

-- import XMonad.Util.EZConfig
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Data.Maybe
import Data.Monoid

import qualified System.Posix as Posix
import System.IO
import System.Posix.Env(setEnv, unsetEnv)
import Data.IORef
import System.Directory

import System.Posix.Types

import Prelude hiding(log)

--import Keys

myShell = "/bin/zsh"
myTerminal      = "urxvt -e tmux new"
myTerminal2     = "xterm"

myBorderWidth   = 2
myModMask       = mod4Mask
myNumlockMask   = mod2Mask

myWorkspaces    = zipWith (++) (map show $ [1..22]) (( map (": "++)
                  ["browser"
                  ,"pidgin"
                  ,"music"
                  ,"misc"
                  ,"emacs"
                  ,"misc"
                  ,"download"
                  ,"misc"
                  ,"email"
                  ,"misc"] ) ++ repeat "" )

myNormalBorderColor  = "#300080"
myFocusedBorderColor = "#FF9000"

setVolume v = spawn $ "pactl set-sink-volume 1 -- " ++ v ++"%"

basicKeys scratchpads log conf =
  [ ("M-S-<Return>",  spawn $ XMonad.terminal conf)
  -- Volume control
 , ("M-<KP_Add>"        , setVolume "+5" )
 , ("M-<KP_Subtract>"   , setVolume "-5" )
 , ("M-S-<KP_Add>"      , setVolume "60" )
 , ("M-S-<KP_Subtract>" , setVolume "30" )
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
  , ("M-q" ,broadcastMessage ReleaseResources >> restart "xmonad" True)
  , ("M-S-l", spawn "xscreensaver-command -lock" )
  , ("M-`", namedScratchpadAction scratchpads "kuake")
  , ("M-c", namedScratchpadAction scratchpads "canto")
  , ("M-f", namedScratchpadAction scratchpads "feedreader")
  , ("M-r", namedScratchpadAction scratchpads "run program")
  , ("M-a", namedScratchpadAction scratchpads "krusader")
  , ("M-w", namedScratchpadAction scratchpads "gnome-commander")
  , ("M-g", namedScratchpadAction scratchpads "ghci")
  , ("M-S-`", resetScratchpadWindow scratchpads)
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
    [ ( "xfce4-panel"           , 1 ) -- (Program to execute, delay afterwards)
    , ( "nautilus -n"           , 1 )
    , ( "pidgin -n"             , 3 )
    , ( "xchat"                 , 2 )
    , ( "claws-mail"            , 2 )
    , ( "emacs"                 , 2 )
    , ( "firefox"               , 3 )
    , ( "liferea"               , 5 )
    , ( "skype"                 , 3 )
    , ( "clementine"            , 10)
    , ( "ktorrent"              , 5 )
--    , ( "nicotine"              , 5 )
    , ( "keepassx"              , 5 )
    , ( "jdownloader"           , 20)
    ]
  delayProgram :: Int -> String -> X ()
  delayProgram time prog = spawn $ "sleep " ++ show time ++ "; " ++ prog
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

myLayout = --mouseResizableTile{ draggerType = BordersDragger } |||
           mouseResizableTile ||| -- {draggerType = BordersDragger} |||
           simpleTabbed

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
                 ]
               , map (\(cn, ws) -> className =? cn --> doShift (workspace ws) )
                 [ ( "Firefox"    , 1)
                 , ( "Namoroka"   , 1)
                 , ( "Xchat"      , 2)
                 , ( "Pidgin"     , 2)
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
                 , ( "Kmail"      , 10)
                 , ( "Keepassx"   , 11)
                 , ( "Liferea"    , 12)
                 , ( "Skype"      , 14)
                 ]
               , [title =? "JDownloader" --> doShift (workspace 7)]
               ]
  where workspace n = myWorkspaces !! (n-1)
        byName doX names = (\name -> className =? name --> doX ) <$> names

myFocusFollowsMouse = False

myLogHook = ewmhDesktopsLogHook
            >> setWMName "LG3D" -- solves the empty java windows issue

-- Send WM_TAKE_FOCUS
takeTopFocus = withWindowSet $ maybe (setFocusX =<< asks theRoot) takeFocusX . W.peek

atom_WM_TAKE_FOCUS      = getAtom "WM_TAKE_FOCUS"
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

pipeWriter filename chan = do
  ifM ( andM ([ doesFileExist , (fmap Posix.isNamedPipe . Posix.getFileStatus)]    <*> [filename])) -- file exists and is a named pipe
    -- then
    (withFile filename WriteMode $ \pipe -> forever $ do
      out <- readChan chan
      hPutStrLn pipe out
      hFlush pipe)
    -- else
    (forever $ readChan chan)

main = do
  -- chan <- newChan
  -- forkIO $ pipeWriter "/home/uart14/.xmonad/pipemon.pipe" chan
  -- forkIO $ osdFun2
  spawn "amixet set Master 60%"
  setEnv "SHELL" myShell True -- set Shell to be fish
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

krusaderKuake = NS { name = "krusader"
                     , cmd = "krusader"
                     , query = appName =? "krusader"
                     , hook = krusaderHook
                     }

gnomeCommanderKuake = NS { name = "gnome-commander"
                     , cmd = "gnome-commander"
                     , query = appName =? "gnome-commander"
                     , hook = krusaderHook
                     }

feedreaderKuake = NS { name = "feedreader"
                     , cmd = "feedreader"
                     , query = appName =? "feedreader"
                     , hook = cantoHook
                     }



scratchpads = [ termKuake "kuake" (kuakeShellCmd "SPad" myShell ) kuakeHook
              , termKuake "ghci"  (kuakeShellCmd "ghci" "ghci" ) kuakeHook
              , termKuake "canto" "canto" cantoHook
              , termKuake "htop" "htop" cantoHook
              , feedreaderKuake
              , runProgramKuake
              , krusaderKuake
              , gnomeCommanderKuake
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

