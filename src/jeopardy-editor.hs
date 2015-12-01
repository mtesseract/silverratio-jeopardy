-- Copyright (C) 2013-2015 Moritz Schulte <mtesseract@silverratio.net>

module Main (main) where

import Control.Monad                      -- For 'when'.
import Data.IORef                   -- We store program state in an IORef
import Data.Maybe                   -- And of course the Maybe type.

import Graphics.UI.Gtk              -- The GUI Toolkit
import Control.Monad.Trans(liftIO)  -- We need liftIO for Gtk
import qualified Data.Map as M      -- We implement the jeopardy table as a
import System.Environment           -- Commandline Arguments.
import System.Directory
import System.IO.Temp
import System.IO                      -- For "withFile"
import qualified Data.ByteString as B
import Data.List                      -- For 'sort'.
import Control.Exception                  -- For 'assert'.

import Jeopardy.Data
import Jeopardy.GUI
import Jeopardy.Utils
import Jeopardy.Compiler
import Paths_silverratio_jeopardy              -- Access to data files.

data EditorState = EditorState { editorFile :: Maybe FilePath,
                                 editorWindow :: Window,
                                 editorBuilder :: Builder,
                                 editorModified :: Bool
                               } 

compileText :: String ->
               String -> FilePath -> IO (Maybe B.ByteString)
compileText name text tmpDir = do
  oldCwd <- getCurrentDirectory
  setCurrentDirectory tmpDir
  textCompiled <- compileOne name text
  setCurrentDirectory oldCwd
  return textCompiled

presentPicture :: Builder -> Pixbuf -> IO ()
presentPicture builder pixbuf = do
  win <- builderGetObject builder castToWindow "winPreview"
  img <- builderGetObject builder castToImage "imgPreview"
  imageSetFromPixbuf img pixbuf
  widgetShowAll win

renderText :: Builder -> String -> IO ()
renderText builder name = do
  text <- retrieveText builder name
  preamble <- retrieveText builder "jdata_preamble"
  oldCwd <- getCurrentDirectory
  let text' = textWrap preamble text oldCwd
  byteString <- withTempDirectory "/tmp" "jitem.tmp"
                  (compileText name text')
  if isJust byteString
    then do pixbuf <- byteStringToPixbuf (fromJust byteString)
            -- FIXME, display error?
            maybe (putStrLn "Failed to convert ByteString into Pixbuf")
                  (presentPicture builder)
                  pixbuf
    else putStrLn "Compilation Failure" -- FIXME, display error.

frobContextMenu :: Builder -> String -> Menu -> IO ()
frobContextMenu builder name menu = do
  menuItem <- menuItemNewWithLabel "Render"
  on menuItem menuItemActivate (renderText builder name)
  menuShellPrepend menu menuItem
  widgetShow menuItem

frobContextMenus :: Builder -> IO ()
frobContextMenus builder = do
  let installSignalFunc prefix suffix = do
        let name = prefix ++ "_" ++ suffix
        textView <- builderGetObject builder castToTextView name
        on textView populatePopup (frobContextMenu builder name)
  mapM_ (installSignalFunc "answer") cardIndex
  mapM_ (installSignalFunc "question") cardIndex
            
-- Connect actions to menu items, for example
jeopardyEditorInit :: IORef EditorState -> Builder -> IO ()
jeopardyEditorInit state' builder = do
  state <- readIORef state'
  window <- builderGetObject builder
              castToWindow "window_editor"
  fileOpen <- builderGetObject builder
                castToMenuItem "editor_menu_file_open"
  fileSave <- builderGetObject builder
                castToMenuItem "editor_menu_file_save"
  fileSaveAs <- builderGetObject builder
                  castToMenuItem "editor_menu_file_save_as"
  fileQuit <- builderGetObject builder
                castToMenuItem "editor_menu_file_quit"
  on fileOpen menuItemActivate $ jeopardyEditorFileOpen state'
  on fileSave menuItemActivate $ jeopardyEditorFileSave state'
  on fileSaveAs menuItemActivate $ jeopardyEditorFileSaveAs state'
  on fileQuit menuItemActivate jeopardyEditorFileQuit
  frobContextMenus builder
  connectToChangedSignals state'
  win <- builderGetObject builder castToWindow "winPreview"
  on win deleteEvent $ liftIO $ widgetHide win >> return True
  winPreview <- builderGetObject builder castToWindow "winPreview"
  widgetModifyBg winPreview StateNormal (Color 0 0 0) -- FIXME!

jeopardyEditor :: Maybe FilePath -> IO ()
jeopardyEditor fname = do
  initGUI
  builder <- builderNew
  gladeFilename <- getDataFileName jeopardyGtkBuilder
  builderAddFromFile builder gladeFilename
  winEditor <- builderGetObject builder castToWindow "window_editor"
  winEditor `on` deleteEvent $ liftIO mainQuit >> return False
  let state' = EditorState { editorFile = Nothing,
                             editorWindow = winEditor,
                             editorModified = False,
                             editorBuilder = builder }
  state <- newIORef state'
  jeopardyEditorInit state builder
  widgetShowAll winEditor
  when (isJust fname) $ editorLoadFile state (fromJust fname)
  mainGUI
  
askForOverwrite :: String -> IO Bool
askForOverwrite filename = return True

jeopardyEditorFileSave :: IORef EditorState -> IO ()
jeopardyEditorFileSave state' = do
  state <- readIORef state'
  let file = editorFile state
      builder = editorBuilder state
  if isNothing file
    then jeopardyEditorFileSaveAs state'
    else do let jdata = buildJData builder
            jeopardyEditorExport builder (fromJust file)

jeopardyEditorFileSaveAs :: IORef EditorState -> IO ()
jeopardyEditorFileSaveAs state' = do
  state <- readIORef state'
  let window = editorWindow state
      builder = editorBuilder state
  fileChooser <- fileChooserDialogNew Nothing
                                      (Just window)
                                      FileChooserActionSave
                                      [ (stockCancel, ResponseCancel),
                                        (stockOk,     ResponseOk    ) ]
  response <- dialogRun fileChooser
  if response == ResponseOk
    then do fileName' <- fileChooserGetFilename fileChooser
            widgetDestroy fileChooser
            if isJust fileName'
              then do let fileName = fromJust fileName'
                      ret <- doesFileExist fileName
                      ok <- if ret
                            then askForOverwrite fileName
                            else return True
                      when ok $ do
                        jeopardyEditorExport builder fileName
                        writeIORef state' $ state { editorFile = Just fileName }
              else putStrLn "Error: no file to save to?"
    else widgetDestroy fileChooser

jeopardyEditorFileQuit :: IO ()
jeopardyEditorFileQuit  = mainQuit

jeopardyEditorRegisterChange :: IORef EditorState -> IO ()
jeopardyEditorRegisterChange state' = do
  state <- readIORef state'
  let newState = EditorState { editorFile = editorFile state,
                               editorWindow = editorWindow state,
                               editorModified = True,
                               editorBuilder = editorBuilder state }
  writeIORef state' newState

connectToBufferChangedSignal :: IORef EditorState -> String -> IO ()
connectToBufferChangedSignal state' name = do
  state <- readIORef state'
  let builder = editorBuilder state
  textview <- builderGetObject builder castToTextView name
  buffer <- textViewGetBuffer textview
  buffer `on` bufferChanged $ jeopardyEditorRegisterChange state'
  return ()

connectToEntryChangedSignal :: IORef EditorState -> String -> IO ()
connectToEntryChangedSignal state' name = do
  state <- readIORef state'
  let builder = editorBuilder state
  entry <- builderGetObject builder castToEntry name
  entry `on` editableChanged $ jeopardyEditorRegisterChange state'
  return ()

connectToChangedSignals :: IORef EditorState -> IO ()
connectToChangedSignals state = do
  connectToEntryChangedSignal state "entry_jdata_name"
  mapM_ (\ idx -> connectToEntryChangedSignal state
                                              ("category_" ++ show idx))
        [1..5]
  mapM_ (\ (category, price) -> do
            let suffix = category ++ show price
            connectToBufferChangedSignal state
                                         ("answer_" ++ suffix)
            connectToBufferChangedSignal state
                                         ("question_" ++ suffix))
    cardIndex'

setText :: Builder -> String -> String -> IO ()
setText builder name content = do
  textview <- builderGetObject builder castToTextView name
  buffer <- textViewGetBuffer textview
  textBufferSetText buffer content

setJItem :: Builder -> String -> (Category, Price) -> String -> IO ()
setJItem builder label' (category, price) content = do
  let label = label' ++ category ++ show price
  setText builder label content

setJItemFlags' :: Builder -> String -> (Category, Price) -> Bool -> IO ()
setJItemFlags' builder name (category, price) state = do
  let widgetName = "flag_" ++ name ++ "_" ++ category ++ show price
  button <- builderGetObject builder castToToggleButton widgetName
  toggleButtonSetActive button state

setJItemFlags :: Builder -> (Category, Price) -> Bool -> IO ()
setJItemFlags builder (category, price) =
  setJItemFlags' builder "double" (category, price)
  
-- This function loads JData into the Editor GUI.
editorLoadJData :: Builder -> JData -> IO ()
editorLoadJData builder jdata = do
  setTextEntry builder "entry_jdata_name" (jdataName jdata)
  setText builder "jdata_preamble" (jdataPreamble jdata)
  -- Load JData categories into GUI.
  mapM_ (\ (idx, value) -> setTextEntry builder ("category_" ++ show idx) value)
        $ zip [1..5] (jdataCategories jdata)
  -- Load JItems into GUI.
  mapM_ (\ (category, price) -> do
            let jitem = M.lookup (category, price) (jdataTable jdata)
            when (isJust jitem) $
              let jitem'     = fromJust      jitem
                  answ       = jitemAnswer   jitem'
                  ques       = jitemQuestion jitem'
                  flagDouble = jitemDouble   jitem'
              in do setJItem builder "answer_" (category, price) answ
                    setJItem builder "question_" (category, price) ques
                    setJItemFlags builder (category, price) flagDouble)
    cardIndex'


editorLoadJDataFromFile :: Builder -> FilePath -> IO ()
editorLoadJDataFromFile builder filename = do
  content <- readFile filename
  let jdata = maybeRead content
  when (isJust jdata) $
    editorLoadJData builder (fromJust jdata)
    -- Error, opening failed, display error

editorLoadFile :: IORef EditorState -> FilePath -> IO ()
editorLoadFile state' fname = do
  state <- readIORef state'
  putStrLn $ "Loading " ++ show fname
  let newState =
        EditorState { editorFile = Just fname,
                      editorWindow = editorWindow state,
                      editorBuilder = editorBuilder state,
                      editorModified = False }
  editorLoadJDataFromFile (editorBuilder state) fname
  writeIORef state' newState

jeopardyEditorFileOpen' :: IORef EditorState -> IO ()
jeopardyEditorFileOpen' state' = do
  state <- readIORef state'
  let parent = editorWindow state
  fileChooser <- fileChooserDialogNew Nothing
                                      (Just parent)
                                      FileChooserActionOpen
                                      [ (stockCancel, ResponseCancel),
                                        (stockOk,     ResponseOk    ) ]
  response <- dialogRun fileChooser
  if response == ResponseOk
    then do fileName <- fileChooserGetFilename fileChooser
            widgetDestroy fileChooser
            maybe (putStrLn "Error: no file to load?")
                  (editorLoadFile state') fileName
    else widgetDestroy fileChooser

jeopardyEditorFileOpen :: IORef EditorState -> IO ()
jeopardyEditorFileOpen state' = do
  state <- readIORef state'
  -- FIXME:
  -- let parent = editorWindow state
  -- when (editorModified state) $ do
  --   putStrLn "Do you want to save the data?"
  --   jeopardyEditorFileSave state'
  jeopardyEditorFileOpen' state'

createTemplateJData :: FilePath -> IO ()
createTemplateJData filename = do
  let templateCategories = map (\ s -> "Category " ++ show s) _Categories
      templateJItems = map (\ key ->
                             let keyCat = fst key
                                 keyPrice = snd key
                             in (key, JItem { jitemAnswer = "Answer",
                                              jitemQuestion = "Question",
                                              jitemDouble = False }))
                         cardIndex'
      templateTable = foldl (\ table ((category, price), jitem) ->
                              M.insert (category, price) jitem table)
                        M.empty templateJItems
      templateJData = JData { jdataName = "Datasheet Name",
                              jdataPreamble = "% \\usepackage{something}",
                              jdataCategories = templateCategories,
                              jdataTable = templateTable }
  exportDataToFile templateJData filename

exportDataToFile :: JData -> FilePath -> IO ()
exportDataToFile jdata filename =
  withFile filename WriteMode
    (\ h -> do
        let table = jdataTable jdata
            keys  = sort $ M.keys table
            printJItem key = do
              let jitem' = M.lookup key table
                  jitem = assert (isJust jitem') $ fromJust jitem'
                  question = jitemQuestion jitem
                  answer = jitemAnswer jitem
                  double = jitemDouble jitem
              hPutStr h $ "    (" ++ show key ++ ",\n\
                          \     JItem {\n\
                          \       jitemAnswer = " ++ show answer ++ ",\n\
                          \       jitemQuestion = " ++ show question ++ ",\n\
                          \       jitemDouble = " ++ show double ++ "\n\
                          \     })"
        assert (not (null keys)) $ return ()
        let firstKeys = init keys
            lastKey   = last keys
        hPutStr h $
          "JData {\n"
          ++ "  jdataName = " ++ show (jdataName jdata) ++ ",\n"
          ++ "  jdataCategories = " ++ show (jdataCategories jdata) ++ ",\n"
          ++ "  jdataPreamble = " ++ show (jdataPreamble jdata) ++ ",\n"
          ++ "  jdataTable = fromList [\n"
        mapM_ (\ key -> do
                  printJItem key
                  hPutStr h ",\n")
          firstKeys
        printJItem lastKey
        hPutStr h $ "\n  ]\n"
          ++ "}")

jeopardyEditorExport :: Builder -> FilePath -> IO ()
jeopardyEditorExport builder filename = do
  jdata <- buildJData builder
  exportDataToFile jdata filename
  putStrLn $ "Saved to " ++ filename

retrieveTextEntry :: Builder -> String -> IO String
retrieveTextEntry builder label = do
  entry <- builderGetObject builder castToEntry label
  entryGetText entry

setTextEntry :: Builder -> String -> String -> IO ()
setTextEntry builder label value = do
  entry <- builderGetObject builder castToEntry label
  entrySetText entry value

retrieveText :: Builder -> String -> IO String
retrieveText builder label = do
  textview <- builderGetObject builder castToTextView label
  buffer <- textViewGetBuffer textview
  iterStart <- textBufferGetStartIter buffer
  iterEnd <- textBufferGetEndIter buffer
  textBufferGetText buffer iterStart iterEnd False

retrieveFlag :: Builder -> String -> String -> IO Bool
retrieveFlag builder name identifier = do
  let widgetName = "flag_" ++ name ++ "_" ++ identifier
  button <- builderGetObject builder castToToggleButton widgetName
  toggleButtonGetActive button

retrieveJItem :: Builder -> Category -> Price -> IO ((Category, Price), JItem)
retrieveJItem builder category price = do
  let identifier = category ++ show price
  answer <- retrieveText builder ("answer_" ++ identifier)
  question <- retrieveText builder ("question_" ++ identifier)
  isDouble <- retrieveFlag builder "double" identifier
  return ((category, price),
          JItem { jitemAnswer = answer, jitemQuestion = question,
                  jitemDouble = isDouble })

retrieveJData :: Builder -> IO [((Category, Price), JItem)]
retrieveJData builder =
  mapM (uncurry (retrieveJItem builder)) cardIndex'

retrieveJDataBuildTable :: Builder -> IO JDataTable
retrieveJDataBuildTable builder = do
  jitems <- retrieveJData builder
  return $ foldl (\ table ((category, price), jitem) ->
                   M.insert (category, price) jitem table)
                 M.empty
                 jitems

retrieveCategories :: Builder -> IO [String]
retrieveCategories builder =
  mapM (\ idx -> retrieveTextEntry builder ("category_" ++ show idx)) [1..5]

buildJData :: Builder -> IO JData
buildJData builder = do
  name <- retrieveTextEntry builder "entry_jdata_name"
  categories <- retrieveCategories builder
  preamble <- retrieveText builder "jdata_preamble"
  table <- retrieveJDataBuildTable builder
  return JData { jdataName = name
               , jdataPreamble = preamble
               , jdataCategories = categories
               , jdataTable = table }

main :: IO ()
main = do
  args <- getArgs
  let argsN = length args
  case args of
    [] -> jeopardyEditor Nothing
    [name] -> jeopardyEditor (Just name)
    ["--create", fname] -> createTemplateJData fname
    _ -> putStrLn "Usage error: invalid command."
