-- Copyright (C) 2013-2015 Moritz Schulte <mtesseract@silverratio.net>

module Jeopardy.Compiler where

import qualified Data.ByteString as B
import System.IO
import System.Process
import System.Exit

import Jeopardy.Utils

ensureNewline :: String -> String
ensureNewline "" = ""
ensureNewline s =
  if (last s) == '\n'
  then s
  else s ++ "\n"

_LaTeXTemplateHead :: String -> String
_LaTeXTemplateHead preamble =
  unlines $
  ["\\documentclass[varwidth=true,convert={density=1200,size=660}]{standalone}",
  "\\usepackage{color}",
  "\\usepackage[utf8]{inputenc}",
  "\\usepackage[T1]{fontenc}",
  "\\usepackage[sc]{mathpazo}",
  "\\usepackage{anyfontsize}",
  "\\usepackage{amscd}",
  "\\usepackage{amsfonts}",
  "\\usepackage{amsmath}",
  "\\usepackage{amssymb}",
  "\\usepackage{amstext}",
  "\\usepackage{amsthm}",
  "\\usepackage{bbm}",
  "\\usepackage{graphicx}",
  preamble,
  "\\begin{document}",
  "\\color{white}",
  "\\pagecolor{black}",
  "\\begin{center}",
  "\\bfseries",
  "\\fontsize{22}{24}\\selectfont"]
 
_LaTeXTemplateFoot :: String
_LaTeXTemplateFoot = unlines
 [ "\\end{center}",
   "\\end{document}" ]
 
textWrap :: String -> String -> FilePath -> String
textWrap preamble content directory = 
  let contentStripped = (trimWhitespaces content)
      content' = if null contentStripped
                 then "(none)"
                 else contentStripped
  in (_LaTeXTemplateHead preamble)
     ++ "\\graphicspath{{" ++ directory ++ "/}}\n"
     ++ content' ++ _LaTeXTemplateFoot

compileOne :: FilePath -> String -> IO (Maybe B.ByteString)
compileOne filename content = do
  let filenameIn  = filename ++ ".tex"
      filenameOut = filename ++ ".png" -- FIXME?
  writeFile (filename ++ ".tex") content
  hIn <- openFile "/dev/null" ReadMode
  hOut <- openFile "/dev/null" WriteMode
  let cp = proc "pdflatex" ["-shell-escape", filenameIn]
  (_, _, _, phandle) <- createProcess $
                          cp { std_in = UseHandle hIn,
                               std_out = UseHandle hOut,
                               close_fds = True }
  res <- waitForProcess phandle
  if res == ExitSuccess
    then do content <- B.readFile filenameOut
            return (Just content)
    else do putStrLn $ "compilation failed with exit code: "
              ++ (show res)
            return Nothing
