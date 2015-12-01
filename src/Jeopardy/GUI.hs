-- Copyright (C) 2013-2015 Moritz Schulte <mtesseract@silverratio.net>

module Jeopardy.GUI where

import Graphics.UI.Gtk                    -- The GUI Toolkit.
import System.IO
import System.Directory
import qualified Data.ByteString as B
-- -- import qualified Data.Map as M
--import System.IO.Temp

-- -- import Jeopardy.Utils
--import Data.Char -- toUpper, ord

-- Transform a picture given as a ByteString into a Pixbuf picture.
byteStringToPixbuf :: B.ByteString -> IO (Maybe Pixbuf)
byteStringToPixbuf bytestring = do
  (filename, handle) <- openTempFile "/tmp" "jeopardyPixbuf.png"
  B.hPut handle bytestring
  hClose handle
  pixbuf <- pixbufNewFromFile filename
  removeFile filename
  return $ Just pixbuf

-- Easy retrival of widgets through Builder.

-- Reorder arguments of builderGetObject.
builderGetObject' :: GObjectClass cls =>
                     (GObject -> cls) -> Builder -> String -> IO cls
builderGetObject' castFunc builder =
  builderGetObject builder castFunc
  
-- Retrieve a plain widget through its name
_GUIWidget :: Builder -> String -> IO Widget
_GUIWidget = builderGetObject' castToWidget

-- Retrieve a Window widget through its name
_GUIWindow :: Builder -> String -> IO Window
_GUIWindow = builderGetObject' castToWindow

-- Retrieve an Entry widget through its name
_GUIEntry :: Builder -> String -> IO Entry
_GUIEntry = builderGetObject' castToEntry

-- Retrieve a TextView widget through its name
_GUITextView :: Builder -> String -> IO TextView
_GUITextView = builderGetObject' castToTextView

-- Retrieve a Label widget through its name
_GUILabel :: Builder -> String -> IO Label
_GUILabel = builderGetObject' castToLabel

-- Retrieve an Image widget through its name
_GUIImage :: Builder -> String -> IO Image
_GUIImage = builderGetObject' castToImage

-- Retrieve a Button widget through its name
_GUIButton :: Builder -> String -> IO Button
_GUIButton = builderGetObject' castToButton

-- Retrieve an EventBox widget through its name
_GUIEventBox :: Builder -> String -> IO EventBox
_GUIEventBox = builderGetObject' castToEventBox

-- Retrieve an Alignment widget through its name
_GUIAlignment :: Builder -> String -> IO Alignment
_GUIAlignment = builderGetObject' castToAlignment

-- Retrieve an HBox widget through its name
_GUIHBox :: Builder -> String -> IO HBox
_GUIHBox = builderGetObject' castToHBox
