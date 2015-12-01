-- Copyright (C) 2013-2015 Moritz Schulte <mtesseract@silverratio.net>

-- This code needs a major cleanup, less imperative programing style,
-- safer programing (eliminate fromJust, etc.), less code duplication,
-- a nicer state handling, etc.
--
-- Feel free to submit patches. -MS

module Main (main) where

import Graphics.UI.Gtk                    -- The GUI Toolkit.
import System.Glib.UTFString
import System.Environment                 -- Commandline Arguments.
import System.Random                      -- For picking players randomly.
import System.Process
import System.IO                          -- Support for file handles.
import System.IO.Temp                     -- We need temporary files.
import System.Exit                        -- ExitCode handling.
import Data.List                          -- List functions like 'nub'.
import Data.IORef                         -- We manage state with IORefs.
import Data.Maybe                         -- The Maybe type.
import Data.Foldable
import qualified Data.Map as M            -- The Map type.
import qualified Data.ByteString as B     -- We also need ByteStrings.
import qualified Codec.Archive.Tar as Tar -- Support for Tar archives.
import Control.Concurrent                 -- For 'forkIO'.
import Control.Exception                  -- For 'assert', 'try'.
import System.IO.Error
import Control.Monad                      -- For 'when'.
import Control.Monad.Trans(liftIO)        -- We need liftIO for Gtk.

import Jeopardy.Data               -- Jeopardy data structures.
import Jeopardy.GUI
import Jeopardy.Utils                       -- General utility functions.
import Paths_silverratio_jeopardy              -- Access to data files.

----------------------
-- Type Definitions --
----------------------

-- 'Player' data structure.
type PlayerID     = Integer
type PlayerName   = String
type PlayerBuzzer = String
data Player       = Player { playerName   :: PlayerName
                           , playerBuzzer :: Maybe PlayerBuzzer
                           } deriving (Read, Show)

-- Mapping: {PlayerID} -> {Player}
type PlayerMap = M.Map PlayerID Player

-- This is the state which contains all the necessary data to restore
-- the current game state.
data State_ = State_ { stateJDataFile_      :: Maybe FilePath
                     , statePlayers_        :: PlayerMap
                     , stateActionLog_      :: [JAction]
                     , stateCurrentDecider_ :: Maybe PlayerID
                     } deriving (Read, Show)

-- Main program state.
data State = State {
  -- Logger MVar
  stateLogger :: MVar (),

  -- State for the Audio Subsystem.
  stateAudio :: AudioState,
  
  -- Players in the game.
  statePlayers   :: PlayerMap,
  
  -- The list of 'jeopardy actions' (e.g., Player P has answered
  -- Question Q of price P correctly).
  stateActionLog :: [JAction],
  
  -- This is the {Player} -> {Points} mapping. It can be entirely
  -- (re)computed via a list of JActions.
  statePoints :: PointTable,
  
  -- We need a random number generator.
  stateRandom :: StdGen,
                           
  -- BEGIN: GUI Toolkit specific members
  stateBuilder :: Builder,
  -- END:   GUI Toolkit specific members

  -- To keep track of players who have buzzed.
  stateBuzzed :: [PlayerID],

  -- This is the compiled 'Jeopardy Data' (i.e. the table of
  -- answers/questions).
  stateJData :: JDataCompiled,
  
  -- Filename from which we have read the Jeopardy Data.
  stateJDataFile :: Maybe FilePath,
  
  -- The current phase of the Jeopardy state machine.
  statePhase     :: GamePhase,

  -- Statefile. If set, we use this for saving snapshots of the game
  -- state. This state can be used to restore the game.
  stateStateFile :: Maybe FilePath,

  -- When we need the notion of 'current player'. e.g. during player
  -- initialization or when a player has buzzed during a query.
  stateCurrentPlayer :: Maybe PlayerID,

  -- If Just, this contains the player ID of the player who has picked
  -- the last JItem.
  stateCurrentDecider :: Maybe PlayerID,

  -- During a query, this is the 'current jeopardy item'.
  stateCurrentJItem :: Maybe (Category, Price),

  -- The timeout handler during the query phase.
  stateTimeoutHandler :: Maybe HandlerId,

  -- The name of the 'screen' which is currently displayed on the
  -- 'game board'.
  stateGameScreen :: String,

  -- When the timeout is reached while waiting for a player's answer,
  -- this flag is set. 
  stateTimeoutReached :: Bool,

  -- Controls wether we need to pick a random player or not during
  -- PrepareQuery.
  --stateNeedRandomPlayer :: Bool,

  -- If we are currently in 'Double Jeopardy' mode.
  stateDoubleJeopardy :: Bool,

  -- If debugging (i.e. verbosity) is enabled or not.
  stateDebug :: Bool }

-- Type used for points.
type Points = Integer

-- This is the map {playerID} -> {Points}.
type PointTable = M.Map PlayerID Points

-- A "JAction" describes what player has answered which question
-- correctly/wrong.
data JAction    = JAction {
  -- ID of the player. ID == -1 means 'timeout reached'
  jactionPlayerID :: PlayerID,
  -- The question:
  jactionCategory :: Category,
  jactionPrice    :: Price,
  -- If it was answered correct or not.
  jactionCorrect  :: Bool } deriving (Read, Show)

-- The GamePhase describes the 'current state' of the game.
data GamePhase =
  PhaseNothing                | -- Not a valid phase.
  PhasePreGame                | -- No game started yet.
  PhasePrepareGame            | -- Currently waiting for game init
                                -- data.
  PhaseGamePrepared           | -- Game init data has been provided.
  PhasePlayerInit             | -- Currently initializing player list.
  PhasePreQuery               | -- No JItem as been chosen so yet.
  PhaseInQuery                | -- JItem chosen, waiting for buzzers.
  PhaseQueryTimeout           |
  PhasePostGame               | -- Game is over.
  PhaseQueryBuzzed            | -- We have received buzzer events
                                -- during query.
  PhaseShowAnswer             | -- Currently showing the answer.
  PhaseShowAnswerAfterTimeout | -- Currently showing answer, after timeout.
  PhaseGameRestored           | -- Game state has been restored.
  PhasePreDoubleJeopardy      | -- Prepare Double Jeopardy.
  PhaseInDoubleJeopardy       | -- Ask Double Jeopardy. 
  PhasePostQuery                -- Query phase is over.
  deriving (Eq, Show, Read)

-- These are the 'game actions'. The set of actions which are allowed
-- to be triggered depends on the current GamePhase.
data JeopardyAction = 
  AdminGameNew                  |
  AdminGameStop                 |
  AdminGameQuit                 |
  AdminHelpAbout                |
  AdminNewGameStart             |
  AdminNewGameCancel            |
  AdminNewGameSelectDataFile    |
  AdminNewGameSelectStateFile   |
  AdminGameEnter                |
  PrepareQuery                  |
  AdminRestoreGame              |
  AdminShowAnswer               |
  AdminContinue                 |
  AdminPickRandomPlayer         |
  AdminAnswerCorrect            |
  AdminCancelPlayerInit         |
  AdminPlayerInit               |
  GameQueryTimeout              |
  GameBuzzed PlayerID           |
  DigitPressed PlayerID         |
  GameEnterQuery Category Price |
  QueryAnswered Bool            |
  GameOver
  deriving (Read, Show)

-- Suitable for initializing JDataCompiled values.
emptyJData :: JDataCompiled
emptyJData = JDataCompiled { jdataName_       = ""
                           , jdataCategories_ = []
                           , jdataTable_      = M.empty
                           }

----------------------------
-- Game Related Constants --
----------------------------

-- These are GUI-specific names of some important widgets.
cardContainerPrefix :: String
cardContainerPrefix = "eventbox_"

cardPrefix :: String
cardPrefix          = "label_"

-- Audio Player Command
audioPlayer :: String
audioPlayer = "play"

-- The audio files to play on events.
audioFileBuzzer :: String
audioFileBuzzer  = "Button.ogg"

audioFileTimeout :: String
audioFileTimeout = "Timeout.ogg"

audioFileClosing :: String
audioFileClosing = "Closing.ogg"

audioFileTheme :: String
audioFileTheme   = "Theme.ogg"

audioFileCorrect :: String
audioFileCorrect = "Ding.ogg"

audioFileWrong :: String
audioFileWrong   = "Wrong.ogg"

audioFileStart :: String
audioFileStart   = "Woosh.ogg"

audioFileDouble :: String
audioFileDouble  = "DoubleJeopardy.ogg"

audioFileRandom :: String
audioFileRandom  = "DingLing.ogg"

bootScreenImage :: String
bootScreenImage  = "Images/Boot.jpg"

buzzerImageName :: String
buzzerImageName  = "Images/Buzzed.png"

-- Maximum number of players allowed.
maxPlayers :: Integer
maxPlayers = 4

-- In miliseconds.
timeoutDisplayBuzzing :: Int
timeoutDisplayBuzzing = 2000

timeoutQuery :: Int
timeoutQuery = 20000

timeoutRandomCB1 :: Int
timeoutRandomCB1 = 1000

timeoutRandomCB2 :: Int
timeoutRandomCB2 = 2000

-- Std Files
devNull :: String
devNull = "/dev/null"

-------------------------------
-- GUI Toolkit Encapsulation --
-------------------------------

-- Color theme.
colorGameBg :: Color
colorGameBg  = Color     0     0     0

colorCardBg :: Color
colorCardBg  = Color     0     0 40535

colorCardBg' :: Color
colorCardBg' = Color     0     0  9000

colorCardFg :: Color
colorCardFg  = Color 65535 65535 65535

colorGameFg :: Color
colorGameFg  = Color 65535 65535 65535

-- Define the colors of players.
colorPlayers :: [Color]
colorPlayers  = [ Color 65535     0     0 -- 1st player: Red
                , Color     0 65535     0 -- 2nd player: Green
                , Color 65535 65535     0 -- 3rd player: Yellow
                , Color 65535     0 65535 -- 4th player: Purple
                ]

-- Same as above, only darker.
colorPlayers' :: [Color]
colorPlayers' = [ Color 35535     0     0
                , Color     0 35535     0
                , Color 35535 35535     0
                , Color 35535     0 35535
                ] 

------------
-- Logger --
------------

logInfo :: State -> String -> IO ()
logInfo state msg =
  withMVar (stateLogger state) (\ _ -> putStrLn msg)

logErr :: State -> String -> IO ()
logErr state msg =
  withMVar (stateLogger state) (\ _ -> hPutStrLn stderr msg)

------------------------------------------------
-- GUI Sensitivity Control of certain  widget --
------------------------------------------------

-- The 'Jeopardy State' includes some kind of 'Context' for the
-- underlying GUI Toolkit. In case of Gtk this is simply the
-- "Builder", which we use from time to time to retrieve widgets.
type GUICtx = Builder

-- GUI Controls
data GUIControl =
  ButtonPickRandomPlayer     |
  ButtonAnsweredCorrectly    |
  ButtonAnsweredWrong        |
  ButtonShowAnswer           |
  ButtonContinue             |
  ButtonJItem Category Price |
  AdminJItemTable
  deriving (Eq, Show, Read)

-- Convert GUIControl values into their according widget names.
_GUIControlName :: GUIControl -> String
_GUIControlName guictrl =
  case guictrl of
    ButtonPickRandomPlayer     -> "button_random_player"
    ButtonAnsweredCorrectly    -> "admin_button_answer_correct"
    ButtonAnsweredWrong        -> "admin_button_answer_wrong"
    ButtonShowAnswer           -> "button_show_answer"
    ButtonContinue             -> "button_continue"
    ButtonJItem category price -> "admin_jitem_" ++ category ++ show price
    AdminJItemTable            -> "admin_jitem_table"

-- Manage Sensitivity of GUI widgets.
_GUISetSensitivity :: IORef State -> GUIControl -> Bool -> IO ()
_GUISetSensitivity state' guictrl bool = do
  state <- readIORef state'
  let builder = stateBuilder state
      wName = _GUIControlName guictrl
  widget <- _GUIWidget builder wName
  widgetSetSensitive widget bool

_GUISensitivityOn :: IORef State -> GUIControl -> IO ()
_GUISensitivityOn state' guictrl =
  _GUISetSensitivity state' guictrl True

_GUISensitivityOff :: IORef State -> GUIControl -> IO ()
_GUISensitivityOff state' guictrl =
  _GUISetSensitivity state' guictrl False

------------------------------------------------------------

-----------------
-- GUI Helpers --
-----------------

-- Display a popup window containing an error message.
_GUIError :: Window -> String -> String -> IO ()
_GUIError parent _ msg = do
  dialog <- dialogNew
  windowSetTransientFor dialog parent
  _ <- on dialog deleteEvent (liftIO $ return True) -- FIXME: True or False?
  set dialog [ windowTitle := "" ]
  contentArea <- dialogGetUpper dialog
  label <- labelNew $ Just msg
  boxPackStart contentArea label PackGrow 10
  widgetShowAll contentArea
  _ <- dialogAddButton dialog stockOk ResponseOk
  _ <- dialogRun dialog
  widgetDestroy dialog

-- Display message, ask user Ok/Cancel question.
_GUIAskOkCancel :: Window -> String -> String -> IO Bool
_GUIAskOkCancel parent title msg = do
  dialog <- dialogNew
  windowSetTransientFor dialog parent
  _ <- on dialog deleteEvent (liftIO $ return True) -- FIXME: True or False?
  set dialog [ windowTitle := title ]
  contentArea <- dialogGetUpper dialog
  label <- labelNew $ Just msg
  boxPackStart contentArea label PackGrow 10
  widgetShowAll contentArea
  _ <- dialogAddButton dialog stockOk ResponseOk
  _ <- dialogAddButton dialog stockCancel ResponseCancel
  res <- dialogRun dialog
  widgetDestroy dialog
  return (res == ResponseOk)

-- Display a File Chooser Dialog. PARENT is the parent window.
_GUIfileDialog :: Window -> String -> Bool -> IO (Maybe FilePath)
_GUIfileDialog parent title openMode = do
  let mode = if openMode
             then FileChooserActionOpen
             else FileChooserActionSave
      title' = Just title
      parent' = Just parent
      buttons = [ (glibToString stockCancel, ResponseCancel)
                , (glibToString stockOk, ResponseOk) ]
  fileChooser <- fileChooserDialogNew title' parent' mode buttons
  res <- dialogRun fileChooser
  if res == ResponseOk
    then do fileName <- fileChooserGetFilename fileChooser
            widgetDestroy fileChooser
            return fileName
    else do widgetDestroy fileChooser
            return Nothing

-- Display a File Chooser Dialog for opening a file.
_GUIfileDialogOpen :: Window -> IO (Maybe FilePath)
_GUIfileDialogOpen parent = _GUIfileDialog parent "Open" True

-- Display a File Chooser Dialog for saving a file.
_GUIfileDialogSaveAs :: Window -> IO (Maybe FilePath)
_GUIfileDialogSaveAs parent = _GUIfileDialog parent "Save File" False

-- newWidgetName is expected to denote a widget of type
-- 'Alignment'. The currently displayed (Alignment) widget in the
-- game_board is replaced with the one specified here.
_GUIFrobGameBoard :: IORef State -> String -> IO ()
_GUIFrobGameBoard state' newWidgetName = do
  state <- readIORef state'
  let builder = stateBuilder state
  gameBoard <- _GUIWindow builder "game_board"
  currentWidget <- _GUIAlignment builder (stateGameScreen state)
  newWidget <- _GUIAlignment builder newWidgetName
  containerRemove gameBoard currentWidget
  containerAdd gameBoard newWidget
  writeIORef state' $ state { stateGameScreen = newWidgetName }

-- Install a callback handler for events. Install CALLBACK for widget
-- NAME on SIGNAL. CAST has to be the correct Gtk casting function
-- which is to be used for builderGetObject.
_GUIInstallCB :: (GObjectClass cls) =>
                 IORef State -> String -> (GObject -> cls) ->
                 Signal cls callback -> callback ->
                 IO (ConnectId cls)
_GUIInstallCB state' name cast signal callback = do
  state <- readIORef state'
  let builder = stateBuilder state
  obj <- builderGetObject builder cast name
  on obj signal callback

------------------------------
-- GUI Initialization/Setup --
------------------------------
  
-- Enter the GUI Main loop.
_GUIStart :: IO ()
_GUIStart = mainGUI

-- Initialize the GUI Toolkit. Return the created GUI Context.
_GUIInit :: IO GUICtx
_GUIInit = do
  _ <- initGUI
  builder <- builderNew
  gladeFilename <- getDataFileName jeopardyGtkBuilder
  builderAddFromFile builder gladeFilename
  return builder

-- Shutdown GUI and quit application.
_GUIQuit :: IORef State -> IO ()
_GUIQuit _ = mainQuit

_GUIDetachChilds :: Builder -> [(String, String)] -> IO ()
_GUIDetachChilds builder list = do
  let detachChild (windowName, childName) = do
        window <- _GUIWindow builder windowName
        widget <- _GUIWidget builder childName
        containerRemove window widget
  mapM_ detachChild list

_GUIFrobColors :: Builder ->
                  [(String, StateType, Maybe Color, Maybe Color)] -> IO ()
_GUIFrobColors builder list = do
  let detachChild (wName, stateType, colorFg, colorBg) = do
        widget <- _GUIWidget builder wName
        forM_ colorFg $ widgetModifyFg widget stateType
        forM_ colorBg $ widgetModifyBg widget stateType
  mapM_ detachChild list

-- Setup Menu actions.
_GUISetupMenuActions :: IORef State -> IO ()
_GUISetupMenuActions state' = do
  -- This function installs the handlers to be called on activation of
  -- the menu items.
  let installCB (name, action) =
        _GUIInstallCB state' name castToMenuItem
                      menuItemActivate (_STM action state')
  -- Install the handlers for the given menu items.
  mapM_ installCB
    [ ("admin_game_new",  AdminGameNew),
      ("admin_game_stop", AdminGameStop),
      ("admin_game_quit", AdminGameQuit),
      ("menu_item_about", AdminHelpAbout) ]
  
-- Setup Button actions.
_GUISetupButtonActions :: IORef State -> IO ()
_GUISetupButtonActions state' = do
  -- This function installs the handlers to be called on activation of
  -- the buttons.
  let installCB (name, action) =
        _GUIInstallCB state' name castToButton
                      buttonActivated (_STM action state')
  -- Install the handlers
  mapM_ installCB
    [ ("button_random_player",        AdminPickRandomPlayer),
      ("admin_button_answer_correct", QueryAnswered True),
      ("admin_button_answer_wrong",   QueryAnswered False),
      ("button_select_data_file",     AdminNewGameSelectDataFile),
      ("button_select_statefile",     AdminNewGameSelectStateFile),
      ("button_start_game",           AdminNewGameStart),
      ("button_cancel_new_game",      AdminNewGameCancel),
      ("player_init_cancel",          AdminCancelPlayerInit),
      ("button_show_answer",          AdminShowAnswer),
      ("button_continue",             AdminContinue)]
  -- Now for the JItem Buttons in the Admin CP.
  let installCB' (category, price) = do
        let suffix = category ++ show price
            name = "admin_jitem_" ++ suffix
            cb = _STM (GameEnterQuery category price) state'
        _GUIInstallCB state'
                      name castToButton buttonActivated cb
  mapM_ installCB' cardIndex'

  
-- Setup actions, handlers; change colors of widgets, etc.
_GUISetup :: IORef State -> IO ()
_GUISetup state' = do
  state <- readIORef state'
  let builder = stateBuilder state
  -- Setup Menu actions.
  _GUISetupMenuActions state'
  -- Setup Button actions.
  _GUISetupButtonActions state'
  -- Install handlers which manage sensitivity of buttons in 'New
  -- Game'-dialogue (the 'Start' button shall only be clickable when a
  -- non-zero number of player names has been entered and when a JDC
  -- file has been specified).
  let installHandler idx =
        let wName = "new_game_player_" ++ show idx
            cb = newGameDataChanged state'
        in _GUIInstallCB state' wName castToEntry editableChanged cb
  mapM_ installHandler [1..maxPlayers]
  -- Detach childs of the windows (we don't need so many toplevel
  -- windows, in many cases we only need the content of the toplevel
  -- windows).
  _GUIDetachChilds builder [ ("answerwin", "answerscreen"),
                             ("window3", "buzzerscreen"),
                             ("win_query", "queryscreen"),
                             ("window_boot", "box_boot"),
                             ("game_board", "gamescreen"),
                             ("win_ranking", "box_ranking"),
                             ("win_doublejeopardy", "screen_doublejeopardy"),
                             ("window_random", "randomscreen") ]
  -- Modify Window Colors.
  _GUIFrobColors builder
    [ ("game_board",
       StateNormal, Just colorGameFg, Just colorGameBg),
      ("randomscreen",
       StateNormal, Just colorGameFg, Just colorGameBg),
      ("box_question_preview",
       StateNormal, Just colorGameFg, Just colorGameBg),
      ("box_boot",
       StateNormal, Just colorGameFg, Just colorGameBg),
      ("screen_doublejeopardy",
       StateNormal, Just colorGameFg, Just colorGameBg),
      ("box_ranking",
       StateNormal, Just colorGameFg, Just colorGameBg) ]
  --
  -- Prepare game_board.
  --
  window1 <- _GUIWindow builder "game_board"
  bootBox <- _GUIAlignment builder "box_boot"
  windowSetDeletable window1 False
  windowSetTitle window1 "Jeopardy Game Board"
  containerAdd window1 bootBox
  -- Set 'Boot Image'.
  bootImage <- _GUIImage builder "boot_image"
  getDataFileName bootScreenImage >>= imageSetFromFile bootImage
  -- Set 'Buzzer Image'
  buzzerImage <- _GUIImage builder "image_buzzer"
  getDataFileName buzzerImageName >>= imageSetFromFile buzzerImage
  -- imageSetFromFile buzzerImage imgName
  -- Modify colors of player & points labels in gameboard.
  setPlayerColors builder colorPlayers
  clearCurrentPlayer builder

  --
  -- Prepare Admin Control Panel.
  -- 
  adminWindow <- _GUIWindow builder "window2"
  windowSetTitle adminWindow "Jeopardy Admin Control Panel"
  
  _ <- _GUIInstallCB state' "window2" castToWindow deleteEvent $
         liftIO $ _STM AdminGameQuit state' >> return False
  _ <- _GUIInstallCB state' "game_board" castToWindow deleteEvent $
         liftIO $ return True
  _ <- _GUIInstallCB state' "dialog_new_game" castToWindow deleteEvent $
         liftIO $ _STM AdminNewGameCancel state' >> return True
  _ <- _GUIInstallCB state' "window2" castToWindow keyPressEvent $
         tryEvent $ eventBuzzerPressed' state'
  _ <- _GUIInstallCB state' "window_player_init" castToWindow
         keyPressEvent $ tryEvent $ playerInitialBuzzer state'
  _ <- _GUIInstallCB state' "window_player_init" castToWindow
         keyPressEvent $ tryEvent $ eventBuzzerPressed' state'

  mapM_ (_GUISensitivityOff state')
    [ ButtonPickRandomPlayer,
      ButtonAnsweredCorrectly,
      ButtonAnsweredWrong,
      ButtonShowAnswer,
      ButtonContinue ]

  _GUISensitivityOff state' AdminJItemTable
  
-- Reset the ame board, i.e. 
_GUIResetGameBoard :: IORef State -> IO ()
_GUIResetGameBoard state' = do
  state <- readIORef state'
  let builder = stateBuilder state
  -- Change colors of the card widgets on the game board.
  mapM_ (\ suf -> do
            eventbox <- _GUIEventBox builder (cardContainerPrefix ++ suf)
            label <- builderGetObject builder
                     castToLabel (cardPrefix ++ suf)
            widgetModifyBg eventbox StateNormal colorCardBg
            widgetModifyFg label StateNormal colorCardFg)
    cardIndex
  -- Reset Player/Points info.
  mapM_ (\ idx -> do
            setPlayerName builder idx Nothing
            setPlayerPoints builder idx Nothing)
    [1..maxPlayers]

doubleJeopardyCB :: IORef State -> IO Bool
doubleJeopardyCB state' = do
  _GUISensitivityOn  state' ButtonContinue
  return False

_GUIAdminContinue :: IORef State -> IO ()
_GUIAdminContinue state' = do
  state <- readIORef state'
  let builder = stateBuilder state
  mapM_ (_GUISensitivityOff state') [ButtonContinue]
  -- Clear image in Admin CP.
  img <- _GUIImage builder "admin_question_preview"
  imageClear img
  _GUIFrobGameBoard state' "gamescreen"

-- Ask for data file, ask for players, show game window.
_GUIGameNew :: IORef State -> IO ()
_GUIGameNew state' = do
  state <- readIORef state'
  let builder = stateBuilder state
  _GUILabel builder "data_file_to_load" >>= flip labelSetText ""
  _GUILabel builder "statefile_to_use" >>= flip labelSetText ""
  parent <- _GUIWindow builder "window2"
  dialog <- _GUIWindow builder "dialog_new_game"
  windowSetTransientFor dialog parent
  windowSetModal dialog True
  widgetShow dialog
  newGameDataChanged state'

setCurrentPlayer :: IORef State -> PlayerID -> Player -> IO ()
setCurrentPlayer state' pID player = do
  state <- readIORef state'
  let builder = stateBuilder state
      pName = playerNameEscaped player
      pColor = colorPlayers !! (fromIntegral pID - 1)
      text = pName
  label <- _GUILabel builder "label_current_player"
  logInfo state $ "setCurrentPlayer Color " ++ show pColor
  widgetModifyFg label StateNormal pColor
  labelSetMarkup label $
    "<span weight='bold' font='30'>" ++ text ++ "</span>"
    
setCurrentPlayerTimeout :: IORef State -> IO ()
setCurrentPlayerTimeout state' = do
  state <- readIORef state'
  let builder = stateBuilder state
  label <- _GUILabel builder "label_current_player"
  widgetModifyFg label StateNormal colorGameFg
  labelSetMarkup label "<span weight='bold' font='30'>T I M E O U T</span>"
    
clearCurrentPlayer :: Builder -> IO ()
clearCurrentPlayer builder = do
  label <- _GUILabel builder "label_current_player"
  widgetModifyFg label StateNormal (Color 0 0 0)
  labelSetMarkup label "<span weight='bold' size='large'></span>"
    
_GUIGameBuzzed :: IORef State -> PlayerID -> Player -> IO ()
_GUIGameBuzzed state' pID player = do
  audioSilenceBackground state'
  audioPlayOnce state' audioFileBuzzer
  state <- readIORef state'
  let builder = stateBuilder state
      pColor = colorPlayers !! (fromIntegral pID - 1)
  _ <- _GUIWindow builder "game_board"
  label <- _GUILabel builder "buzzer_screen_player"
  widgetModifyFg label StateNormal pColor
  labelSetMarkup label $
    "<span weight='bold' font='30'>" ++
    playerNameEscaped player ++ "</span>"
  _GUIFrobGameBoard state' "buzzerscreen"
  setCurrentPlayer state' pID player

_GUICancelGame :: IORef State -> IO ()
_GUICancelGame _ = return ()
  
_GUIShowAbout :: IORef State -> IO ()
_GUIShowAbout state' = do
  state <- readIORef state'
  let builder = stateBuilder state
  about <- builderGetObject builder castToAboutDialog "window_about"
  widgetShowAll about
  _ <- dialogRun about
  widgetHide about
  
_GUIInitPlayer :: IORef State -> IO ()
_GUIInitPlayer state' = do
  state <- readIORef state'
  assert (isJust (stateCurrentPlayer state)) $ return ()
  let builder = stateBuilder state
      playerID' = stateCurrentPlayer state
      playerID = assert (isJust playerID') $ fromJust playerID'
      playerTable = statePlayers state
      -- We assume it is a valid PlayerID!
      player' = M.lookup playerID playerTable
      player = assert (isJust player') $ fromJust player'
  win <- _GUIWindow builder  "window_player_init"
  labelName <- _GUILabel builder  "player_init_name"
  labelSetText labelName $ playerName player
  parent <- _GUIWindow builder "window2"
  windowSetTransientFor win parent
  windowSetModal win True
  widgetShow win

_GUICancelPlayerInit :: IORef State -> IO ()
_GUICancelPlayerInit state' = do
  state <- readIORef state'
  let builder = stateBuilder state
  win <- _GUIWindow builder "window_player_init"
  widgetHide win

_GUINewGameStart :: IORef State -> IO ()
_GUINewGameStart state' = do
  state <- readIORef state'
  let builder = stateBuilder state
  win <- _GUIWindow builder "dialog_new_game"
  widgetHide win

_GUICollectPlayerNames :: IORef State -> IO [String]
_GUICollectPlayerNames state' = do
  state <- readIORef state'
  let builder = stateBuilder state
      getNameFunc idx = do
        let entryName = "new_game_player_" ++ show idx
        entry <- _GUIEntry builder entryName
        entryGetText entry
      names = map getNameFunc [1..maxPlayers]
  names' <- sequence names
  return $ filter ("" /=) names'

_GUICancelNewGame :: IORef State -> IO ()
_GUICancelNewGame state' = do
  state <- readIORef state'
  let builder = stateBuilder state
  dialog <- _GUIWindow builder "dialog_new_game"
  widgetHide dialog

_GUIEnterDoubleJeopardy :: IORef State -> IO ()
_GUIEnterDoubleJeopardy state' = do
  _GUIFrobGameBoard state' "screen_doublejeopardy"
  -- Manage Sensitivity
  _GUISensitivityOff state' AdminJItemTable
  _ <- timeoutAdd (doubleJeopardyCB state') 1000
  return ()
  
-- Displays the query screen on the  game board.
_GUIEnterQuery :: IORef State -> Bool -> IO ()
_GUIEnterQuery state' _ = do
  state <- readIORef state'
  assert (isJust (stateCurrentJItem state)) $ return ()
  -- Extract JItem data.
  let builder = stateBuilder state
      jdata = stateJData state
      _jitem = stateCurrentJItem state
      (category, jID) = assert (isJust _jitem) $ fromJust _jitem
      jitem' = M.lookup (category, jID) (jdataTable_ jdata)
      jitem = assert (isJust jitem') $ fromJust jitem'
      questionBS = jitemQuestion_ jitem
      answerBS = jitemAnswer_ jitem
  -- Reset.
  clearCurrentPlayer builder
  -- Convert image data into Pixbufs.
  questionPB' <- byteStringToPixbuf questionBS
  answerPB' <- byteStringToPixbuf answerBS
  assert (isJust questionPB' && isJust answerPB') $ return ()
  let answerPB = assert (isJust answerPB') $ fromJust answerPB'
      questionPB = assert (isJust questionPB') $ fromJust questionPB'
  -- Load images into widgets.
  -- gameBoard <- _GUIWindow builder "game_board"
  img <- _GUIImage builder "jitem_answer"
  imgPreview <- _GUIImage builder "admin_question_preview"
  imageSetFromPixbuf img answerPB
  imageSetFromPixbuf imgPreview questionPB
  -- Update Category name.
  categoryLabel <- _GUILabel builder "query_category_name"
  let categoryIndex = categoryIdx category
      categoryName = jdataCategories_ (stateJData state) !! categoryIndex
      categoryNameEscaped = escapeMarkup categoryName
  labelSetMarkup categoryLabel $
    "<span foreground='white' size='x-large' weight='bold'>"
    ++ categoryNameEscaped ++ "</span>"
  -- Display query screen.
  _GUIFrobGameBoard state' "queryscreen"
  newState <- readIORef state'
  -- Disable JItem table
  _GUISensitivityOff state' AdminJItemTable
  -- Add timeout handler
  handlerId <- timeoutAdd (_STMGameQueryTimeout state'
                           (category, jID)) timeoutQuery
  writeIORef state' $ newState { stateTimeoutHandler = Just handlerId }
  logMsg' state' $ "Query: " ++ show (category, jID)

-- Change the color of the card defined by CATEGORY and PRICE.
_GUIDisableJItemCard :: IORef State ->
                        (Category, Price) -> PlayerID -> IO ()
_GUIDisableJItemCard state' (category, price) player = do
  state <- readIORef state'
  let builder = stateBuilder state
      cardBoxName = cardContainerPrefix ++ category ++ show price
      color = if player == -1
              then colorCardBg'
              else colorPlayers' !! (fromIntegral player - 1)
  cardBox <- _GUIEventBox builder cardBoxName
  widgetModifyBg cardBox StateNormal color

_GUIGameBuzzed' :: IORef State -> IO ()
_GUIGameBuzzed' state' = do
  _GUIFrobGameBoard state' "queryscreen"
  mapM_ (_GUISensitivityOn state') [ ButtonAnsweredCorrectly,
                                     ButtonAnsweredWrong ]

_GUIGameSelectDataFile :: IORef State -> IO (Maybe FilePath)
_GUIGameSelectDataFile state' = do
  state <- readIORef state'
  let builder = stateBuilder state
  parentWindow <- _GUIWindow builder "dialog_new_game"
  res <- _GUIfileDialogOpen parentWindow
  when (isJust res) $ do
    label <- _GUILabel builder "data_file_to_load"
    labelSetText label (fromJust res)
  return res

_GUIGameSelectStateFile :: IORef State -> IO (Maybe FilePath)
_GUIGameSelectStateFile state' = do
  state <- readIORef state'
  let builder = stateBuilder state
  parentWindow <- _GUIWindow builder "dialog_new_game"
  res <- _GUIfileDialogSaveAs parentWindow
  when (isJust res) $ do
    label <- _GUILabel builder "statefile_to_use"
    labelSetText label (fromJust res)
  return res

newGamePlayerData :: IORef State -> Integer -> IO String
newGamePlayerData state' idx = do
  state <- readIORef state'
  let builder = stateBuilder state
  entry <- _GUIEntry builder ("new_game_player_" ++ show idx)
  entryGetText entry

newGameDataChanged :: IORef State -> IO ()
newGameDataChanged state' = do
  state <- readIORef state'
  let builder = stateBuilder state
      sizes' = map (\ idx -> do
                       let pName = newGamePlayerData state' idx
                       fmap length pName)
                 [1..maxPlayers]
  sizes <- sequence sizes'
  buttonStart <- _GUIButton builder "button_start_game"
  if (sum sizes > 0) && isJust (stateJDataFile state)
    then do widgetSetSensitive buttonStart True
            writeIORef state' $
              state { statePhase = PhaseGamePrepared }
    else do widgetSetSensitive buttonStart False
            writeIORef state' $
              state { statePhase = PhasePreGame }

setLabelFg :: Builder -> String -> Color -> IO ()
setLabelFg builder name color = do
  label <- _GUILabel builder name
  widgetModifyFg label StateNormal color

setPlayerColors :: Builder -> [Color] -> IO ()
setPlayerColors builder colors =
  mapM_ (\ (color, i) -> do
            let labelPlayer = "label_player" ++ show i
                labelPlayerPoints =
                  "label_points_player" ++ show i
            setLabelFg builder labelPlayer       color
            setLabelFg builder labelPlayerPoints color) $
    zip colors ([1..] :: [Int])

setPlayerName :: Builder -> PlayerID -> Maybe Player -> IO ()
setPlayerName builder playerID player = do
  label <- _GUILabel builder $ "label_player" ++ show playerID
  if isJust player
    then do let pName = playerNameEscaped (fromJust player)
            labelSetMarkup label $
              "<span weight='bold' size='x-large'>" ++ pName ++ "</span>"
         -- foreground='white' 
    else labelSetMarkup label ""

setPlayerPoints :: Builder -> PlayerID -> Maybe Integer -> IO ()
setPlayerPoints builder playerID points = do
  label <- _GUILabel builder $ "label_points_player" ++ show playerID
  if isJust points
    then labelSetMarkup label $
         "<span weight='bold' size='x-large'>" ++
         show (fromJust points) ++ "</span>"
         --  foreground='white'
    else labelSetMarkup label ""

setCategoryInfo :: Builder -> Integer -> String -> IO ()
setCategoryInfo builder idx _category = do
  -- This is a hack. It makes sure that the Game board is not
  -- misaligned in case some categories are unnamed.
  let category' = if null _category then " " else _category
      category = escapeMarkup category'
  label <- _GUILabel builder $ "label_category" ++ show idx
  labelSetMarkup label $
    "<span size='xx-large' foreground='white' weight='bold'>"
    ++ category ++ "</span>"

addCategoryInfo :: IORef State -> IO ()
addCategoryInfo state' = do
  state <- readIORef state'
  let builder   = stateBuilder state
      jdata = stateJData state
      categories = jdataCategories_ jdata
  mapM_ (uncurry (setCategoryInfo builder)) $
    zip [1..] categories

addPlayerInfo :: IORef State -> IO ()
addPlayerInfo state' = do
  state <- readIORef state'
  let players   = statePlayers state
      playerIDs = M.keys players
      pointsMap = statePoints state
      builder   = stateBuilder state
  -- Add new info
  mapM_ (\ idx -> do
            let player = M.lookup idx players
                points = M.lookup idx pointsMap
            setPlayerName builder idx player
            setPlayerPoints builder idx points) playerIDs

-- Setup point table in State such that for every player the points
-- will be set to zero.
initPointTable :: IORef State -> IO ()
initPointTable state' = do
  state <- readIORef state'
  let players   = statePlayers state
      playerIDs = M.keys players
      newPoints = foldr (\ idx pointTable ->
                          M.insert idx 0 pointTable)
                        M.empty playerIDs
  writeIORef state' $ state { statePoints = newPoints }

playerNameEscaped :: Player -> String
playerNameEscaped player = escapeMarkup (playerName player)

printPlayers :: IORef State -> IO ()
printPlayers state' = do
  state <- readIORef state'
  let playerMap = statePlayers state
      playerIDs = M.keys playerMap
  logInfo state "Players:"
  mapM_ (\ pID ->
          let player' = M.lookup pID playerMap
              player  = assert (isJust player') $ fromJust player'
              pName  = playerName player
          in logInfo state $ "Player " ++ show pID ++ ": " ++ pName)
    playerIDs
  
_GUIEnterGame :: IORef State -> IO ()
_GUIEnterGame state' = do
  -- Reset card colors and player/points info.
  _GUIResetGameBoard state'
  state <- readIORef state'
  let builder = stateBuilder state
  writeIORef state' $ state { statePhase = PhasePreQuery }
  -- Hide Player Init Window
  playerInit <- _GUIWindow builder "window_player_init"
  widgetHide playerInit
  _GUIFrobGameBoard state' "gamescreen"
  logMsg state "Game created"
  printPlayers state'
  initPointTable state'
  addPlayerInfo state'
  addCategoryInfo state'
  _GUISensitivityOn state' ButtonPickRandomPlayer
  _GUISensitivityOff state' AdminJItemTable

_GUIRestoreGame :: IORef State -> IO ()
_GUIRestoreGame state' = do
  state <- readIORef state'
  writeIORef state' $ state { statePhase = PhasePreQuery }
  -- Hide Player Init Window
  _GUIFrobGameBoard state' "gamescreen"
  logMsg state "Game restored"
  let playerMap = statePlayers state
      playerIDs = M.keys playerMap
  logInfo state "Players:"
  mapM_ (\ pID ->
          let player' = M.lookup pID playerMap
              player  = assert (isJust player') $ fromJust player'
          in logInfo state $
               "Player " ++ show pID ++ ": " ++ playerNameEscaped player)
    playerIDs
  addPlayerInfo state'
  addCategoryInfo state'

-- Updates the player/points data on the game board from the current
-- state.
_GUIUpdateGameBoard :: IORef State -> IO ()
_GUIUpdateGameBoard state' = do
  state <- readIORef state'
  let players   = statePlayers state
      playerIDs = M.keys players
      pointsMap = statePoints state
      builder   = stateBuilder state
  -- Add new info
  mapM_ (\ idx -> do
            let player = M.lookup idx players
                points = M.lookup idx pointsMap
            setPlayerName builder idx player
            setPlayerPoints builder idx points) playerIDs

_GUIUpdateAdminPanelJData :: IORef State -> IO ()
_GUIUpdateAdminPanelJData state' = do
  state <- readIORef state'
  let builder = stateBuilder state

  mapM_ (\ (name', suffix) -> do
            let labelName = "label_category_" ++ suffix
                name = escapeMarkup name'
            label <- _GUILabel builder labelName
            labelSetMarkup label $
              "<span weight='bold'>" ++ name ++ "</span>") $
    zip (jdataCategories_ (stateJData state)) _Categories

  label <- _GUILabel builder "label_jdata_name"
  labelSetMarkup label $
    "<span weight='bold' size='large'>" ++ jdataName_ (stateJData state) ++ "</span>"

------------------------
-- Core State Machine --
------------------------

gameEnterQuery :: IORef State -> Category -> Price -> IO ()
gameEnterQuery state' category jID = do
  state <- readIORef state'
  let jdata = jdataTable_ (stateJData state)
      jitemName = (category, jID)
      jitem' = M.lookup jitemName jdata
      jitem = assert (isJust jitem') $ fromJust jitem'
  if jitemDouble_ jitem
    then do -- We got a Double Jeopardy!
            audioPlayOnce state' audioFileDouble
            _GUIEnterDoubleJeopardy state'
            newState <- readIORef state'
            writeIORef state' $
              newState { statePhase = PhasePreDoubleJeopardy,
                         stateCurrentJItem = Just jitemName,
                         stateTimeoutReached = False,
                         stateDoubleJeopardy = True }
    else do -- Normal, i..e non-Double Jeopardy, JItem.
            writeIORef state' $
              state { statePhase = PhaseInQuery,
                      stateCurrentJItem = Just jitemName,
                      stateTimeoutReached = False }
            _GUIEnterQuery state' False
            audioPlayBackgroundFade state' audioFileTheme

queryAnswered :: IORef State -> Bool -> IO ()
queryAnswered state' correct = do
  state <- readIORef state'
  let builder = stateBuilder state
      playerID = head (stateBuzzed state)
      jitem' = stateCurrentJItem state
      jitem = assert (isJust jitem') $ fromJust jitem'
      -- Assemble a JAction
      jaction = JAction { jactionPlayerID = playerID,
                          jactionCategory = fst jitem,
                          jactionPrice    = snd jitem,
                          jactionCorrect  = correct }
  clearCurrentPlayer builder
  executeAction state' jaction
  newState <- readIORef state'
  if correct
    then queryAnsweredCorrectly state'
    else if stateDoubleJeopardy newState
         then queryAnsweredWrongDouble state'
         else queryAnsweredWrong state'

-- Answer was correct.
queryAnsweredCorrectly :: IORef State -> IO ()
queryAnsweredCorrectly state' = do
  state <- readIORef state'
  -- Play music.
  audioPlayOnce state' audioFileCorrect
  -- Remove timeout handler for this query.
  let handler'  = stateTimeoutHandler state
      currentP' = stateCurrentPlayer state
      currentP  = assert (isJust currentP') $ fromJust currentP'
  forM_ handler' timeoutRemove
  -- Clear other players who also buzzed
  writeIORef state' $
    state { stateBuzzed = [],
            --stateNeedRandomPlayer = False,
            stateTimeoutHandler = Nothing,
            stateCurrentDecider = Just currentP }
  -- Enter AdminShowAnswer
  _STM AdminShowAnswer state'

-- Answer was wrong.
queryAnsweredWrong :: IORef State -> IO ()
queryAnsweredWrong state' = do
  state <- readIORef state'
  -- Play music.
  audioPlayOnce state' audioFileWrong
  let otherBuzzers = tail (stateBuzzed state)
  if (not . null) otherBuzzers
     -- Other players have buzzed.
    then do let nextPlayerID = head otherBuzzers
                players = statePlayers state
                player' = M.lookup nextPlayerID players
                player = assert (isJust player') $ fromJust player'
                pName = playerName player
                buzzerCB = _STMGameBuzzed' state' --nextPlayerID
            logMsg state $ "Player " ++ pName ++ " has previously buzzed!"
            logMsg state $ "Ask " ++ pName ++ " now for answer!"
            -- Handle next buzzer in the queue.
            _GUIGameBuzzed state' nextPlayerID player
            _ <- timeoutAdd buzzerCB timeoutDisplayBuzzing
            -- Frob sensitivity.
            mapM_ (_GUISensitivityOff state') [ButtonAnsweredCorrectly,
                                               ButtonAnsweredWrong]
            -- Stay in PhaseQueryBuzzed.
            newState <- readIORef state'
            writeIORef state' $
              newState { stateBuzzed = otherBuzzers,
                         stateCurrentPlayer = Just nextPlayerID }
         -- No other buzzers pressed.
    else do mapM_ (_GUISensitivityOff state') [ButtonAnsweredCorrectly,
                                               ButtonAnsweredWrong]
            if stateTimeoutReached state
               -- Timeout reached while we were handling the previous
               -- buzzer event. Time out now.
              then queryTimeoutReached state' False
                   -- Continue normally.
              else writeIORef state' $
                     state { statePhase = PhaseInQuery,
                             stateBuzzed = [] }

-- Answer was wrong during Double Jeopardy.
queryAnsweredWrongDouble :: IORef State -> IO ()
queryAnsweredWrongDouble state' = do
  state <- readIORef state'
  -- Play music.
  audioPlayOnce state' audioFileWrong
  -- Manage sensitivity.
  mapM_ (_GUISensitivityOff state') [ButtonAnsweredCorrectly, ButtonAnsweredWrong]
  mapM_ (_GUISensitivityOn state') [ButtonShowAnswer]
  -- Remove timeout handler for the current double jeopardy. Note:
  -- timeout handler might have been run+removed in the meantime!
  let timeout' = stateTimeoutHandler state
  forM_ timeout' timeoutRemove
  -- Update state.
  writeIORef state' $
    state { statePhase = PhaseQueryTimeout, -- FIXME!! hack. Rename
                                            -- this phase to something
                                            -- more general.
            --stateNeedRandomPlayer = False,
            stateTimeoutHandler = Nothing,
            stateBuzzed = [],
            stateTimeoutReached = False }

_GUIAdminShowAnswer :: IORef State -> JItemCompiled -> IO ()
_GUIAdminShowAnswer state' jitem = do
  state <- readIORef state'
  let builder = stateBuilder state
      answerBS = jitemQuestion_ jitem
  answerPB' <- byteStringToPixbuf answerBS
  let answerPB = assert (isJust answerPB') $ fromJust answerPB'
  img <- _GUIImage builder "jitem_question"
  imageSetFromPixbuf img answerPB  
  _GUIFrobGameBoard state' "answerscreen"
  mapM_ (_GUISensitivityOff state') [ ButtonAnsweredCorrectly
                                    , ButtonAnsweredWrong
                                    , ButtonShowAnswer
                                    ]
  mapM_ (_GUISensitivityOn state') [ ButtonContinue ]

adminShowAnswer :: IORef State -> IO ()
adminShowAnswer state' = do
  state <- readIORef state'
  let phase = statePhase state
      jdata = stateJData state
      currentJItem' = stateCurrentJItem state
      currentJItem = assert (isJust currentJItem') $
                       fromJust currentJItem'
      (category, jID) = currentJItem
      jitem' = M.lookup (category, jID) (jdataTable_ jdata)
      jitem = assert (isJust jitem') $ fromJust jitem'
  -- FIXME: There's a bug here wrt sensitivity of the PickRandom button(?).
  if phase == PhaseQueryTimeout -- && (stateDoubleJeopardy state))
    then if stateDoubleJeopardy state
         then writeIORef state' $ state { statePhase = PhaseShowAnswer }
         else writeIORef state' $ state { statePhase = PhaseShowAnswerAfterTimeout }
    else writeIORef state' $ state { statePhase = PhaseShowAnswer }
  _GUIAdminShowAnswer state' jitem
  
hasPlayerAlreadyBuzzed :: State -> PlayerID -> Bool
hasPlayerAlreadyBuzzed state pID =
  let matches = filter (pID ==) (stateBuzzed state)
  in (not . null) matches

gameBuzzed :: IORef State -> PlayerID -> IO ()
gameBuzzed state' pID = do
  state <- readIORef state'
  if hasPlayerAlreadyBuzzed state pID
    then logMsg state $ "Ignored buzzer event from player " ++ show pID
    else gameBuzzed' state' pID

gameBuzzed' :: IORef State -> PlayerID -> IO ()
gameBuzzed' state' pID = do
  state <- readIORef state'
  let player' = lookupPlayer state pID
      player = assert (isJust player') $ fromJust player'
      pName = playerName player
      buzzers = stateBuzzed state
      buzzerCB = _STMGameBuzzed' state' --pID
  logMsg state $ pName ++ " has pressed the button"
  -- Keep track of buzzer press.
  addBuzzEvent state' pID
  if (not . null) buzzers
     -- We have already received a previous buzzer press.
    then logInfo state $ "too late (but remembered), "
           ++ playerName player
         -- First buzzer press in this query phase. Change game
         -- screen.
    else do _GUIGameBuzzed state' pID player
            logMsg state $ "Ask " ++ pName ++ " for answer!"
            _ <- timeoutAdd buzzerCB timeoutDisplayBuzzing
            newState <- readIORef state'
            writeIORef state' $
              newState { statePhase = PhaseQueryBuzzed,
                         stateCurrentPlayer = Just pID }

_GUIQueryTimeoutReached :: IORef State -> IO ()
_GUIQueryTimeoutReached state' = do
  -- Display timeout hint.
  setCurrentPlayerTimeout state'
  mapM_ (_GUISensitivityOn state') [ButtonShowAnswer]
  mapM_ (_GUISensitivityOff state') [ButtonAnsweredCorrectly,
                                     ButtonAnsweredWrong]

queryTimeoutReached :: IORef State -> Bool -> IO ()
queryTimeoutReached state' isDouble = do
  -- FIXME: restart music on wrong answer? Would be good, but I'm not
  -- sure how to do it.
  state <- readIORef state'
  let currentDecider = stateCurrentDecider state
  writeIORef state' $
    state { stateTimeoutHandler   = Nothing
          , statePhase            = PhaseQueryTimeout
          , stateCurrentDecider   = if isDouble then currentDecider else Nothing
          , stateBuzzed           = []
          , stateTimeoutReached    = True -- FIXME, correct?
          }
  audioSilenceBackground state'
  audioPlayOnce state' audioFileTimeout
  -- Create new JAction for this timeout event.
  jaction <- createTimeoutJAction state'
  executeAction state' jaction
  newState <- readIORef state'
  let actionLog = stateActionLog newState
  writeIORef state' $
    newState { stateActionLog = actionLog ++ [jaction] }
  _GUIQueryTimeoutReached state'

createTimeoutJAction :: IORef State -> IO JAction
createTimeoutJAction state' = do
  state <- readIORef state'
  let jitem' = stateCurrentJItem state
      jitem = assert (isJust jitem') $ fromJust jitem'
      (category, price) = jitem
      jactionNew = JAction { jactionPlayerID = -1,
                             jactionCategory = category,
                             jactionPrice = price,
                             jactionCorrect = False }
  return jactionNew

adminNewGameStart :: IORef State -> IO ()
adminNewGameStart state' = do
  state <- readIORef state'
  let builder = stateBuilder state
  dialog <- _GUIWindow builder "dialog_new_game"
  -- Assumed for (PhaseGamePrepared, AdminNewGameStart):
  -- stateJDataFile is Just a filename, (wrong: stateplayer contains a
  -- non-zero list of uninitialized players.)
  let jdataFile' = stateJDataFile state
      filename = assert (isJust jdataFile') $ fromJust jdataFile'
  playerNames <- _GUICollectPlayerNames state'
  res <- loadJData state' filename
  if res
    then do let playerMap = createPlayerMap playerNames
                uninitPlayer' = nextUninitPlayer playerMap
                uninitPlayer = assert (isJust uninitPlayer') $
                                 fromJust uninitPlayer'
                uninitPlayerID = fst uninitPlayer
            _GUIUpdateAdminPanelJData state'
            _GUINewGameStart state'
            -- Set current phase to Player Initialization, record the
            -- player which is about to become initialized next.
            newState <- readIORef state'
            writeIORef state' $
              newState { statePhase = PhasePlayerInit,
                         statePlayers = playerMap,
                         stateCurrentPlayer = Just uninitPlayerID }
            -- Execute AdminPlayerInit action.
            _STM AdminPlayerInit state'
    else do logErr state "loading JData failed!"
            _GUIError dialog "Jeopardy Error" "Failed to load JData file"

debugMsg :: IORef State -> String -> IO ()
debugMsg state' msg' = do
  state <- readIORef state'
  when (stateDebug state) $ do
    let msg = "[DEBUG: " ++ msg' ++ "]"
    logErr state msg

-- Being in STATE', process ACTION.
_STM :: JeopardyAction -> IORef State -> IO ()
_STM action state' = do
  state <- readIORef state'
  let builder = stateBuilder state
      phase = statePhase state
  -- Debugging.
  debugMsg state' $
    "STM (phase = " ++ show phase ++ ", action = " ++ show action ++ ")"
  case action of
    AdminGameNew -> do
      when (phase /= PhasePreGame) $
         -- Game already running.
        _GUICancelGame state'
      -- Create new 'empty' game state.
      -- Show new game board, etc.
      _GUIGameNew state'
    AdminGameStop -> do
      let reallyStopMsg = "Game already running. Sure you want to stop game?"
      if phase /= PhasePreGame
        then do adminWin <- _GUIWindow builder "window2"
                res <- _GUIAskOkCancel adminWin "Confirmation required" reallyStopMsg
                when res $ stopGame state'
        else stopGame state'
    AdminGameQuit -> do
      -- FIXME: To do.
      -- if phase /= PhasePreGame
      --   then return () -- display really-quit-game-window?
      --   else return ()
      jeopardyAdminGameQuit state'
      _GUIQuit state'
    AdminHelpAbout ->
      _GUIShowAbout state'
    AdminNewGameSelectDataFile ->
      assert (phase == PhaseGamePrepared || phase == PhasePreGame) $ do
        filename <- _GUIGameSelectDataFile state'
        when (isJust filename) $
          -- Save new data filename in state.
          writeIORef state' $ state { stateJDataFile = filename }
        -- Recheck sensitity of 'Start' button
        newGameDataChanged state'
    AdminNewGameSelectStateFile ->
      assert (phase == PhaseGamePrepared || phase == PhasePreGame) $ do
        filename <- _GUIGameSelectStateFile state'
        when (isJust filename) $
          writeIORef state' $ state { stateStateFile = filename }
        newGameDataChanged state'
    AdminRestoreGame -> assert (phase == PhaseNothing) $ do
      _GUIUpdateAdminPanelJData state'
      writeIORef state' $ state { statePhase = PhaseGameRestored }
      _STM PrepareQuery state'
    AdminNewGameStart ->
      assert (phase == PhaseGamePrepared && isJust (stateJDataFile state)) $
        adminNewGameStart state'
    -- The New Game Preparation Dialog has been cancelled.
    AdminNewGameCancel ->
      _GUICancelNewGame state'
    AdminGameEnter -> assert (phase == PhasePlayerInit) $ do
      audioPlayOnce state' audioFileStart
      writeIORef state' $
        state { statePhase = PhasePreQuery,
                stateCurrentPlayer = Nothing }
      saveState state'
      _GUIEnterGame state'
      logMsg state "Begin by picking a random player"
    -- Initialize next uninitialized player.
    AdminPlayerInit -> assert (phase == PhasePlayerInit || phase == PhasePreGame) $ do
      let player' = nextUninitPlayer (statePlayers state)
      if isNothing player'
        -- All players initialized, enter game mode.
        then _STM AdminGameEnter state'
        -- One more to go.
        else do let playerID = fst (fromJust player')
                writeIORef state' $
                  state { stateCurrentPlayer = Just playerID }
                _GUIInitPlayer state'
    AdminCancelPlayerInit -> assert (phase == PhasePlayerInit) $ do
      _GUICancelPlayerInit state'
      writeIORef state' $ state { statePhase = PhasePreGame
                                , stateJDataFile = Nothing
                                , stateJData = emptyJData }
    AdminPickRandomPlayer -> assert (phase == PhasePreQuery) $ do
      let players   = statePlayers state
          playerIDs = M.keys players
          playersN  = length playerIDs
          (r', newGen) = random (stateRandom state) :: (Int, StdGen)
          r = r' `mod` playersN
          randomID = playerIDs !! r
          randomPlayer' = M.lookup randomID players
          randomPlayer = assert (isJust randomPlayer') $ fromJust randomPlayer'
  
      -- Update PRNG.
      writeIORef state' $ state { stateRandom = newGen,
                                  stateCurrentDecider = Just randomID }
      -- Frob game window to show a 'Picking Random Player...'
      -- message.
      _GUIFrobGameBoard state' "randomscreen"
      _ <- timeoutAdd (randomPlayerCB state' randomPlayer) timeoutRandomCB1
      logMsg state $
        "Randomly picked: " ++ playerName randomPlayer
      mapM_ (_GUISensitivityOff state') [ButtonPickRandomPlayer]
    GameQueryTimeout -> assert (phase == PhaseInQuery
                                || phase == PhaseInDoubleJeopardy
                                || phase == PhaseQueryBuzzed) $ do
      logMsg state "Timeout reached!"
      case phase of
        PhaseQueryBuzzed ->
          -- Remember timeout, but don't do anything now. This might
          -- be handled later when all players answer the query wrong.
          writeIORef state' $
            state { stateTimeoutReached = True, stateTimeoutHandler = Nothing }
        PhaseInQuery ->
          -- Timeout reached without any buzzing activity. Handle the
          -- timeout now.
          queryTimeoutReached state' False
        PhaseInDoubleJeopardy ->
          queryTimeoutReached state' True
        _ -> assert False $ return ()
    GameEnterQuery category jID ->
      assert (phase == PhasePreQuery) $
        gameEnterQuery state' category jID
    DigitPressed pID -> do
      debugMsg state' $ "Button received: " ++ show pID
      when ((phase == PhaseInQuery)
            || (phase == PhaseInDoubleJeopardy)
            || (phase == PhaseQueryBuzzed)) $
        _STM (GameBuzzed pID) state'
    GameBuzzed pID -> assert (phase == PhaseInQuery
                              || phase == PhaseInDoubleJeopardy
                              || phase == PhaseQueryBuzzed) $
      if phase == PhaseInDoubleJeopardy
        then do let decider' = stateCurrentDecider state
                    decider  = assert (isJust decider') $ fromJust decider'
                    pMap = statePlayers state
                    player' = M.lookup pID pMap
                    player = assert (isJust player') $ fromJust player'
                    pName = playerName player
                -- Only allow the 'current player' to buzz during the
                -- Double Jeopardy.
                logMsg state $ pName ++ " has pressed the button"
                when (pID == decider) $ gameBuzzedDoubleJeopardy state'
        else gameBuzzed state' pID
    QueryAnswered correct -> assert (phase == PhaseQueryBuzzed) $
      queryAnswered state' correct
    PrepareQuery -> assert (phase == PhaseGamePrepared
                            || phase == PhasePostQuery
                            || phase == PhaseGameRestored) $ do
      logMsg state "Waiting for query choice"
      -- stateNeedRandomPlayer is False if the last answer given was
      -- correct.
      if isNothing (stateCurrentDecider state)
        then do _GUISensitivityOn state' ButtonPickRandomPlayer
                logMsg state "Pick random player"
        else do _GUISensitivityOn state' AdminJItemTable
                let pMap = statePlayers state
                    decider' = stateCurrentDecider state
                    decider  = assert (isJust decider') $ fromJust decider'
                    player' = M.lookup decider pMap
                    player = assert (isJust player') $ fromJust player'
                    pName = playerName player
                logMsg state $ "Player '" ++ pName ++ "' may decide!"
      writeIORef state' $ state { statePhase = PhasePreQuery,
                                  stateDoubleJeopardy = False}
      if phase == PhaseGameRestored
        then _GUIRestoreGame state'
        else saveState state'
    GameOver -> assert (phase == PhasePostQuery) $ do
      audioPlayBackground state' audioFileClosing
      logMsg state "Game over!"
      rankingScreenUpdate state'
      _GUIFrobGameBoard state' "box_ranking"
    AdminShowAnswer -> assert (phase == PhaseQueryTimeout
                               || phase == PhaseQueryBuzzed) $ do
      assert (isJust (stateCurrentJItem state)) $ return ()
      adminShowAnswer state'
    AdminContinue -> assert (phase == PhaseShowAnswerAfterTimeout
                             || phase == PhasePreDoubleJeopardy
                             || phase == PhaseShowAnswer) $
      if phase == PhasePreDoubleJeopardy
        then gameContinueDoubleJeopardy state'
        else gameContinue state'
         
    _ -> return ()

stopGame :: IORef State -> IO ()
stopGame state' = do
  state <- readIORef state'
  -- FIXME: Disable all running timeouts.
  audioSilenceBackground state'
  let handler = stateTimeoutHandler state
  forM_ handler timeoutRemove
  _GUIStop state'

gameBuzzedDoubleJeopardy :: IORef State -> IO ()
gameBuzzedDoubleJeopardy state' = do
  audioSilenceBackground state'
  audioPlayOnce state' audioFileBuzzer
  state <- readIORef state'
  let currentP'   = stateCurrentDecider state
      currentP    = assert (isJust currentP') $ fromJust currentP'
      playerTable = statePlayers state
      player'     = M.lookup currentP playerTable
      player      = assert (isJust player') $ fromJust player'
      buzzerCB    = _STMGameBuzzed' state' 
  _GUIGameBuzzed state' currentP player
  _ <- timeoutAdd buzzerCB timeoutDisplayBuzzing
  addBuzzEvent state' currentP
  newState <- readIORef state'
  writeIORef state' $
    newState { statePhase = PhaseQueryBuzzed,
               stateCurrentPlayer = Just currentP }

gameContinueDoubleJeopardy :: IORef State -> IO ()
gameContinueDoubleJeopardy state' = do
  audioPlayBackgroundFade state' audioFileTheme
  _GUISensitivityOff state' ButtonContinue
  _GUIEnterQuery state' True
  state <- readIORef state'
  writeIORef state' $
    state { statePhase = PhaseInDoubleJeopardy }

gameContinue :: IORef State -> IO ()
gameContinue state' = do
  state <- readIORef state'
  let phase = statePhase state
      jitemsTotal = M.size (jdataTable_ (stateJData state))
      jitemsDone = length (collectJItemsProcessed state)
      gameOver = jitemsDone == jitemsTotal
  -- Enable ButtonPickRandomPlayer in case we need it.
  when (phase == PhaseShowAnswerAfterTimeout && not gameOver) $
    _GUISensitivityOn state' ButtonPickRandomPlayer
  _GUIAdminContinue state'
  newState <- readIORef state'
  if gameOver
    then do writeIORef state' $
              newState { statePhase = PhasePostQuery
                       , stateCurrentJItem = Nothing }
            _STM GameOver state'
    else do writeIORef state' $
              newState { statePhase = PhasePostQuery
                       , stateCurrentPlayer = Nothing
                       , stateCurrentJItem = Nothing }
            _STM PrepareQuery state'

_GUIRankingScreenUpdate :: Builder ->
                           PlayerID -> PlayerName -> String -> Integer -> IO ()
_GUIRankingScreenUpdate builder pID name' points idx = do
  let name = escapeMarkup name'
  nameLabel <- _GUILabel builder $ "label_ranking_player" ++ show idx
  pointsLabel <- _GUILabel builder $ "label_ranking_points_player" ++ show idx
  when (pID >= 0) $ do
    let pColor = colorPlayers !! (fromIntegral pID - 1)
    widgetModifyFg nameLabel StateNormal pColor
    widgetModifyFg pointsLabel StateNormal pColor
  labelSetMarkup nameLabel $ "<span weight='bold' font='30'>" ++ name ++ "</span>"
  labelSetMarkup pointsLabel $ "<span weight='bold' font='30'>" ++ points ++ "</span>"

rankingScreenUpdate :: IORef State -> IO ()
rankingScreenUpdate state' = do
  state <- readIORef state'
  let builder = stateBuilder state
      players   = statePlayers state
      playerIDs = M.keys players
      pointsMap = statePoints state
      clearPointsFunc = _GUIRankingScreenUpdate builder (-1) "" ""
      setPointsFunc pId = do
        let player' = M.lookup pId players
            player = assert (isJust player') $ fromJust player'
            pName = playerName player
            points' = M.lookup pId pointsMap
            points = assert (isJust points') $ fromJust points'
        _GUIRankingScreenUpdate builder pId pName (show points) pId
  -- Clear everything first.
  mapM_ clearPointsFunc [1..maxPlayers]
  -- Add player/points data.
  mapM_ setPointsFunc playerIDs

playerInitialBuzzer :: IORef State -> EventM EKey ()
playerInitialBuzzer state' = do
  s <- eventKeyName
  when (isDigit' (glibToString s)) $ do
    state <- liftIO $ readIORef state'
    let playerID'  = stateCurrentPlayer state
        playerID   = assert (isJust playerID') $
                       fromJust playerID'
        playerMap  = statePlayers state
        playerMap' = associatePlayerBuzzer
                       playerMap playerID (glibToString s)
    liftIO $ do writeIORef state' $
                  state { statePlayers = playerMap' }
                audioPlayOnce state' audioFileCorrect
                _STM AdminPlayerInit state'

resetSensitivity :: IORef State -> IO ()
resetSensitivity state' = do
  mapM_ (_GUISensitivityOff state') [ButtonAnsweredCorrectly,
                                     ButtonAnsweredWrong,
                                     ButtonShowAnswer,
                                     ButtonPickRandomPlayer,
                                     ButtonContinue]
  mapM_ (_GUISensitivityOn state' . uncurry ButtonJItem) cardIndex'
  _GUISensitivityOff state' AdminJItemTable

_GUIClearGameData :: IORef State -> IO ()
_GUIClearGameData state' = do
  state <- readIORef state'
  let builder = stateBuilder state
      labelsToClear = "label_jdata_name" :
                        (map (\ n -> "label_category" ++ show n) ([1..5] :: [Int]) ++
                         map (\ c -> "label_category_" ++ (c : "")) ['a'..'e'])
  -- Clear labels.
  mapM_ (\ name -> do label <- _GUILabel builder name
                      labelSetMarkup label "")
    labelsToClear
  -- Reset Pictures.
  let picturesToClear = ["admin_question_preview", "jitem_answer"]
  mapM_ (\ name -> do image <- _GUIImage builder name
                      imageClear image)
    picturesToClear

_GUIStop :: IORef State -> IO ()
_GUIStop state' = do
  _GUIClearGameData state'
  resetSensitivity state'
  _GUIFrobGameBoard state' "box_boot"
  state <- readIORef state'
  emptyMVar <- newEmptyMVar
  writeIORef state' $
    state { stateLogger = emptyMVar,
            statePhase = PhasePreGame,
            statePlayers = M.empty,
            stateActionLog = [],
            statePoints = M.empty,
            stateBuzzed = [],
            stateJData = emptyJData,
            stateJDataFile = Nothing,
            stateStateFile = Nothing,
            stateCurrentPlayer = Nothing,
            stateCurrentDecider = Nothing,
            stateCurrentJItem = Nothing,
            stateTimeoutHandler = Nothing,
            stateTimeoutReached = False,
            stateDoubleJeopardy = False }

-- returns Nothing if all players are initialized
nextUninitPlayer :: PlayerMap -> Maybe (PlayerID, Player)
nextUninitPlayer pMap =
  let playerUninit = isNothing . playerBuzzer
      uninitPlayers = M.filter playerUninit pMap
  in if M.null uninitPlayers
     then Nothing
     else let key = (head . M.keys) uninitPlayers
              player' = M.lookup key uninitPlayers
              player = assert (isJust player') $ fromJust player'
          in Just (key, player)

createPlayerMap :: [String] -> PlayerMap
createPlayerMap playerNames = 
  foldr (\ (idx, name) playerMap ->
          M.insert idx Player { playerName = name,
                                playerBuzzer = Nothing } playerMap)
        M.empty $ zip [1..] playerNames

-- pID has to be a valid Player ID.
associatePlayerBuzzer :: PlayerMap ->
                         PlayerID -> PlayerBuzzer -> PlayerMap
associatePlayerBuzzer pMap pID pBuzzer =
  let player'  = M.lookup pID pMap
      player   = assert (isJust player') $ fromJust player'
      playerNew = player { playerBuzzer = Just pBuzzer }
    in M.insert pID playerNew pMap
  
addBuzzEvent :: IORef State -> PlayerID -> IO ()
addBuzzEvent state' player = do
  state <- readIORef state'
  writeIORef state' $ state { stateBuzzed = stateBuzzed state ++ [player] }

_STMGameQueryTimeout :: IORef State -> (Category, Price) -> IO Bool
_STMGameQueryTimeout state' _ = do  
  _STM GameQueryTimeout state'
  return False

---------------------------------------
-- Game Independent Helper Functions -- 
---------------------------------------
  
------------------------------------
-- Game Specific Helper Functions --
------------------------------------

-- Lookup Player by it's Player ID.
lookupPlayer :: State -> PlayerID -> Maybe Player
lookupPlayer state pID =
  let players = statePlayers state
  in M.lookup pID players

-- Given a PlayerMap, lookup a player by it's PlayerBuzzer ID.
lookupPlayerByBuzzer :: PlayerMap -> PlayerBuzzer -> Maybe Player
lookupPlayerByBuzzer players buzzer =
  if isDigit' buzzer
  then let playerFilter player =
             let pBuz = playerBuzzer player in
               pBuz == Just buzzer
           playersMatched = M.filter playerFilter players
       in if M.size playersMatched == 1
          then let keys = M.keys playersMatched
               in M.lookup (head keys) playersMatched
          else Nothing
  else Nothing

-- Write log message to Admin Control Panel.
logMsg :: State -> String -> IO ()
logMsg state string = do
  logInfo state $ "LOG: " ++ string
  let builder = stateBuilder state
  -- Rerieve widget
  textview <- _GUITextView builder "protocol"
  textbuffer <- textViewGetBuffer textview
  -- Add message.
  iter <- textBufferGetEndIter textbuffer
  textBufferInsert textbuffer iter $ string ++ "\n"
  -- Scroll Text View.
  textview' <- _GUITextView builder "protocol"
  mark <- textViewGetBuffer textview' >>= textBufferGetInsert
  textViewScrollToMark textview' mark 0.2 (Just (0, 0.5))

logMsg' :: IORef State -> String -> IO ()
logMsg' state' string = do
  state <- readIORef state'
  logMsg state string

randomPlayerCB :: IORef State -> Player -> IO Bool
randomPlayerCB state' player = do
  audioPlayOnce state' audioFileRandom
  state <- readIORef state'
  let pID = playerGetID state player
      builder = stateBuilder state
      pName = playerNameEscaped player
      pColor = colorPlayers !! (fromIntegral pID - 1)
      text = pName
  _ <- timeoutAdd (randomPlayerCB2 state') timeoutRandomCB2
  label <- _GUILabel builder "random_player_name"
  widgetModifyFg label StateNormal pColor
  labelSetMarkup label $
    "<span weight='bold' font='40'>" ++
    text ++ "</span>"
  return False -- Destroy this timeout handler.

randomPlayerCB2 :: IORef State -> IO Bool
randomPlayerCB2 state' = do
  state <- readIORef state'
  let builder = stateBuilder state
  -- Frob game window back
  label <- _GUILabel builder "random_player_name"
  labelSetText label ""
  _GUIFrobGameBoard state' "gamescreen"
  _GUISensitivityOn state' AdminJItemTable
  return False -- Destroy this timeout handler.

_GUIExecuteActionTimeout :: IORef State -> JAction -> IO ()
_GUIExecuteActionTimeout state' action = do
  let category = jactionCategory action
      price    = jactionPrice action
      jitem    = (category, price)
  _GUIDisableJItemCard state' jitem (-1)
  _GUISensitivityOff state' (ButtonJItem category price)
  
_GUIExecuteActionAnswer :: IORef State -> JAction -> IO ()
_GUIExecuteActionAnswer state' action = do
  state <- readIORef state'
  let category = jactionCategory action
      price    = jactionPrice action
      jitem    = (category, price)
      pID      = jactionPlayerID action
  _GUIDisableJItemCard state' jitem pID
  _GUIUpdateGameBoard state'
  _GUISensitivityOff state' (ButtonJItem category price)
  let playerTable = statePlayers state
      playerID    = jactionPlayerID action
  if playerID >= 0
    then do let player' = M.lookup playerID playerTable
                player  = assert (isJust player') $ fromJust player'
                pName   = playerName player
                correct = if jactionCorrect action
                          then "correct"
                          else "wrong"
            logMsg state $
              "User " ++ pName ++ " gave " ++ correct ++ " answer."
    else logMsg state "Timeout reached. FIXME."

isJItemDoubleJeopardy :: State -> (Category, Price) -> Bool
isJItemDoubleJeopardy state (category, price) =
  let jdatatable = jdataTable_ (stateJData state)
      jitem' = M.lookup (category, price) jdatatable
      jitem = assert (isJust jitem') $ fromJust jitem'
  in jitemDouble_ jitem

computeNewPoints :: Integer -> Integer -> Bool -> Bool -> Integer
computeNewPoints oldPoints price isDouble isCorrect =
  if isCorrect
  then oldPoints + (if isDouble then price * 2 else price)
  else oldPoints - price

executeAction :: IORef State -> JAction -> IO ()
executeAction state' action = do
  state <- readIORef state'
  let player = jactionPlayerID action
  if player >= 0
     -- The action is of type 'Player X has answered Y
     -- correctly/wrong'.
    then do let category = jactionCategory action
                price = jactionPrice action
                price' = jidToPrice price
                pointTable = statePoints state
                oldPoints' = M.lookup player pointTable
                oldPoints = assert (isJust oldPoints') $
                              fromJust oldPoints'
                isDouble = isJItemDoubleJeopardy state (category, price)
                isCorrect = jactionCorrect action
                newPoints = computeNewPoints oldPoints price' isDouble isCorrect
                newPointTable = M.insert player newPoints pointTable
            writeIORef state' $
              state { statePoints    = newPointTable,
                      stateActionLog = stateActionLog state ++ [action] }
            _GUIExecuteActionAnswer state' action
         -- It's a 'Timeout action'
    else do writeIORef state' $
              state { stateActionLog = stateActionLog state ++ [action] }
            _GUIExecuteActionTimeout state' action

executeActionLog :: IORef State -> [JAction] -> IO ()
executeActionLog state' = mapM_ (executeAction state')

_STMGameBuzzed' :: IORef State -> IO Bool
_STMGameBuzzed' state' = do
  _GUIGameBuzzed' state'
  -- Only need to modify sensitivity if .. FIXME! not required?
  mapM_ (_GUISensitivityOn state') [ButtonAnsweredCorrectly, ButtonAnsweredWrong]
  return False -- Destroy this timeout handler.

-- Convert a Player into it's PlayerID, based on the data contained in
-- State.
playerGetID :: State -> Player -> PlayerID
playerGetID state player =
  let pName = playerName player
      maplist = M.toList (statePlayers state)
      filterFunc (_, val) = playerName val == pName
      matches = filter filterFunc maplist
  in fst (head matches)

-- Callback for events of buzzer presses.
eventBuzzerPressed' :: IORef State -> EventM EKey ()
eventBuzzerPressed' state' = do
  state <- liftIO $ readIORef state'
  s <- eventKeyName
  let player = lookupPlayerByBuzzer (statePlayers state) (glibToString s)
  when (isJust player) $ do
    -- We have retrieved a valid buzzer press.
    let playerID = playerGetID state (fromJust player)
    liftIO $ _STM (DigitPressed playerID) state'

loadJData :: IORef State -> String -> IO Bool
loadJData state' filename = do
  state <- readIORef state'
  jdata' <- jdataImport filename
  if isJust jdata'
    then do let jdata = fromJust jdata'
            logMsg state $
              "Loaded JData from file '" ++ filename ++ "'" ++
              "[name = '" ++ jdataName_ jdata ++ "']"
            let newState = state { stateJData = jdata,
                                   stateJDataFile = Just filename }
            writeIORef state' newState
            return True
    else return False

-- Quit the game.
jeopardyAdminGameQuit :: IORef State -> IO ()
jeopardyAdminGameQuit = audioSilenceBackground

-- Initialize PRNG.
randomGenInitializer :: IO Int
randomGenInitializer =
  getStdRandom (randomR (1, 100))

-- Main Jeopardy function.
jeopardyMain :: IORef State -> Maybe FilePath -> IO ()
jeopardyMain state' stateFile = do
  state <- readIORef state'
  let builder = stateBuilder state
  logMsg state "Jeopardy running"
  -- Show Game Board & Admin Panel.
  _GUIWindow builder "game_board" >>= widgetShowAll
  _GUIWindow builder "window2" >>= widgetShowAll
  
  res <- if isJust stateFile
            -- Restore game from state.
         then do logMsg state $
                   "Restoring game from state file " ++
                   fromJust stateFile
                 -- Reset card colors and player/points info.
                 _GUIResetGameBoard state'
                 r <- loadStateFile state' (fromJust stateFile)
                 _STM AdminRestoreGame state'
                 return r -- True --r
         else do logMsg state "Chose Game -> New to start a new game"
                 writeIORef state' $
                   state { statePhase = PhasePreGame }
                 return True
  -- Enter GUI event loop
  when res _GUIStart

-- Return a list of all JItem specifiers processed so far.
collectJItemsProcessed :: State -> [(Category, Price)]
collectJItemsProcessed state = 
  let jactions = stateActionLog state
  in nub $ map (\ jaction -> (jactionCategory jaction,
                              jactionPrice jaction)) jactions

-- Initialize PRNG, Audio. Return a new ("vanilla") IORef State value.
initState :: GUICtx -> IO (IORef State)
initState guictx = do
  randInit <- randomGenInitializer
  audioState <- audioInit
  logger <- newMVar ()
  let state = State { stateLogger = logger
                    , stateAudio = audioState
                    , statePoints =  M.empty
                    , stateRandom = mkStdGen randInit
                    , stateBuzzed = []
                    , stateBuilder = guictx
                    , stateJData = emptyJData
                    , statePhase = PhaseNothing
                    , stateJDataFile = Nothing
                    , stateStateFile = Nothing
                    , statePlayers = M.empty
                    , stateCurrentPlayer = Nothing
                    , stateCurrentDecider = Nothing
                    , stateCurrentJItem = Nothing
                    , stateTimeoutHandler = Nothing
                    , stateGameScreen = "box_boot"
                    , stateTimeoutReached = False
                    , stateDoubleJeopardy = False
                    , stateActionLog = []
                    , stateDebug = True }
  newIORef state

-- Initialize game.
jeopardyInit :: IO (IORef State)
jeopardyInit = do
  -- Initialize Gtk+
  guictx <- _GUIInit
  -- Read Glade file
  state' <- initState guictx
  _GUISetup state'
  resetSensitivity state'
  return state'

-------------------------
-- State Import/Export --
-------------------------

-- Write current game state to state file (contained in State).
saveState :: IORef State -> IO ()
saveState state' = do
  state <- readIORef state'
  let statefile = stateStateFile state
  when (isJust statefile) $
    let state_ = State_ { stateJDataFile_ = stateJDataFile state
                        , statePlayers_ = statePlayers state
                        , stateActionLog_ = stateActionLog state
                        , stateCurrentDecider_ = stateCurrentDecider state }
        stateString = show state_
    in writeFile (fromJust statefile) stateString

restoreState :: IORef State -> FilePath -> State_ -> IO Bool
restoreState state' stateFile statePers = do
  state <- readIORef state'
  -- Got state from disk
  let actionLog  = stateActionLog_ statePers
      jdataFile' = stateJDataFile_ statePers
      jdataFile  = assert (isJust jdataFile') $ fromJust jdataFile'
      players    = statePlayers_ statePers
      decider    = stateCurrentDecider_ statePers
  writeIORef state' $ state { stateJDataFile = jdataFile'
                            , stateStateFile = Just stateFile
                            , statePlayers   = players
                            , stateCurrentDecider = decider }
  -- Initialize Point Table.
  initPointTable state'
  res <- loadJData state' jdataFile
  newState <- readIORef state'
  if res
    then do executeActionLog state' actionLog
            if null actionLog
              then do writeIORef state' $
                        newState { stateTimeoutReached = False } -- FIXME?
                      return True
              else do let lastAction = last actionLog
                          pID        = jactionPlayerID lastAction
                      if pID == (-1)
                         -- 'Timeout reached' was last action. 
                        then do writeIORef state' $
                                  newState { stateTimeoutReached   = True }
                                return True
                        else do writeIORef state' $
                                  newState { stateTimeoutReached   = False }
                                -- The last action in a saved state
                                -- must be either a timeout or of type
                                -- 'Player pID answered correctly.
                                assert (jactionCorrect lastAction) $ return ()
                                return True
    else do logErr newState "Failed to load JData File"
            return False
  --return res
  
-- Load game state from state file "stateFile". "state'" must be in
-- PhaseNothing.
loadStateFile :: IORef State -> FilePath -> IO Bool
loadStateFile state' stateFile = do
  state <- readIORef state'
  assert (statePhase state == PhaseNothing) $ return ()
  content <- readFile stateFile
  let statePers' = maybeRead content
  if isJust statePers'
    then do let statePers = fromJust statePers'
            restoreState state' stateFile statePers
    else return False

-----------
-- JData --
-----------
  
-- Import the JItem referenced by (CATEGORY, PRICE) from the directory
-- TMPDIR, store it in JDATA.
jitemImport :: FilePath -> (Category, Integer) ->
               IO JDataCompiled -> IO JDataCompiled
jitemImport tmpDir (category, price) jdata = do
  jdata' <- jdata
  metaContent <- readFile $ tmpDir ++ "/jitem-" ++ category ++ show price
  let meta' = maybeRead metaContent
      meta = assert (isJust meta') $ fromJust meta'
      isDouble = jitemMetaDouble meta
  answer <- B.readFile $
              tmpDir ++ "/jitem-" ++ category ++ show price
              ++ "-answer.png"
  question <- B.readFile $
                tmpDir ++ "/jitem-" ++ category ++ show price
                ++ "-question.png"
  let jitem = JItemCompiled { jitemAnswer_ = answer,
                              jitemQuestion_ = question,
                              jitemDouble_ = isDouble }
      newTable = M.insert (category, price)
                   jitem (jdataTable_ jdata')
  return $ jdata' { jdataTable_ = newTable }

tarExtract :: FilePath -> FilePath -> IO Bool
tarExtract directory filename = do
  res <- try (Tar.extract directory filename) :: IO (Either SomeException ())
  case res of
    Left e  -> do putStrLn $
                    "Exception raised during Tar.extract: " ++ show e -- FIXME, logErr?
                  return False
    Right _ -> return True

-- Read a .jdc (Jeopardy Data Compiled) file, convert it into a
-- JDataCompiled value.
jdataImport :: FilePath -> IO (Maybe JDataCompiled)
jdataImport filename =
  withSystemTempDirectory "tmp-jeopardy.jdc"
    (\ tmpDir -> do
        -- Extract TAR archive into tmpDir
        res <- tarExtract tmpDir filename
        if res
          then do meta <- readFile $ tmpDir ++ "/meta" -- Read/parse metadata file.
                  -- Import all the JItems.
                  jdataNew <- foldr (jitemImport tmpDir)
                                (return (read meta)) cardIndex'
                  return (Just jdataNew)
          else return Nothing)

-----------------
-- Audio Layer --
-----------------

data AudioState_ = AudioState_ { backgroundPHs :: [ProcessHandle] }
type AudioState = MVar AudioState_

audioInit :: IO AudioState
audioInit =
  newMVar AudioState_ { backgroundPHs = [] }

audioPlayOnce' :: State -> FilePath -> IO ThreadId
audioPlayOnce' state filename' =
  forkIO $ do
    filename <- getDataFileName ("Sounds/" ++ filename')
    hDevNull <- openFile devNull ReadWriteMode
    let cp = proc audioPlayer ["-q", filename]
    result <- tryIOError (createProcess $ cp { close_fds = True,
                                               std_in  = UseHandle hDevNull,
                                               std_out = UseHandle hDevNull,
                                               std_err = UseHandle hDevNull })
    ret <- case result of
             Left _ -> return (ExitFailure 1)
             Right (_, _, _, phandle) -> waitForProcess phandle
    when (ret /= ExitSuccess) $
      logErr state "Failed to spawn audio player"
    hClose hDevNull

audioPlayOnce :: IORef State -> FilePath -> IO ()
audioPlayOnce state' filename = do
  state <- readIORef state'
  _ <- audioPlayOnce' state filename
  return ()

audioPlayBackground' :: State -> FilePath -> [String] -> IO ()
audioPlayBackground' state filename' args = do
  let audioState = stateAudio state
  _ <- forkIO $ do
         filename <- getDataFileName ("Sounds/" ++ filename')
         hDevNull <- openFile devNull ReadWriteMode
         let cp = proc audioPlayer (["-q", filename] ++ args)
         (_, _, _, phandle) <- createProcess $ cp { close_fds = True,
                                                    std_in  = UseHandle hDevNull,
                                                    std_out = UseHandle hDevNull,
                                                    std_err = UseHandle hDevNull}
         audioState_ <- takeMVar audioState
         let handles = backgroundPHs audioState_
             handlesNew = handles ++ [phandle]
         putMVar audioState $ audioState_ { backgroundPHs = handlesNew }
         ret <- waitForProcess phandle
         when (ret /= ExitSuccess) $
           logErr state "Failed to spawn audio player"
         hClose hDevNull
  return ()

audioPlayBackground :: IORef State -> FilePath -> IO ()
audioPlayBackground state' filename = do
  state <- readIORef state'
  audioPlayBackground' state filename []

audioPlayBackgroundFade :: IORef State -> FilePath -> IO ()
audioPlayBackgroundFade state' filename = do
  state <- readIORef state'
  let args = ["vol", "0.2", "amplitude" ]
  audioPlayBackground' state filename args

audioSilenceBackground' :: AudioState -> IO ()
audioSilenceBackground' audioState = do
  audioState_ <- takeMVar audioState
  let handles = backgroundPHs audioState_
  mapM_ terminateProcess handles
  putMVar audioState $ audioState_ { backgroundPHs = [] }

audioSilenceBackground :: IORef State -> IO ()
audioSilenceBackground state' = do
  state <- readIORef state'
  audioSilenceBackground' (stateAudio state)

----------
-- Main --
----------
  
-- Main function. Parses command line arguments, starts up the game.
main :: IO ()
main = do
  args <- getArgs
  state <- jeopardyInit
  if length args == 1
    then jeopardyMain state $ Just (head args)
    else jeopardyMain state Nothing
