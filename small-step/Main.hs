module Main where

import System.Environment (getArgs)
import Data.List (intercalate)
import System.IO (readFile, writeFile)
import Text.Regex (Regex, mkRegexWithOpts, subRegex)

import Types (Stmt)
import Memory (showMemory)
import Tests (progAssigments, progMin, progFact, progEuclid)
import Evaluation (Config, executionConfigs)


opening, closing :: String -- html markup
opening = "<div id=\"configs\">"
closing = "</div> <!-- #configs -->"

singleLine, caseInsensitive :: Bool -- regex options
singleLine = False
caseInsensitive = True
configsRegex :: Regex
configsRegex = mkRegexWithOpts (opening  ++ ".*" ++ closing) singleLine caseInsensitive

replaceConfigs :: String -> String -> String
replaceConfigs old new =  subRegex configsRegex old (opening ++ "\n" ++ new ++ "\n" ++ closing)


configToSection :: Config -> String -> String
configToSection (code, mem) time =
  "  <section class=\"snapshot " ++ time ++ "\">" ++
  "    <div class=\"code\">"   ++ (show code)   ++ "</div>" ++
  "    <div class=\"memory\">" ++ (showMemory mem) ++ "</div>" ++
  "  </section>\n"

configsToHtml :: [Config] -> String
configsToHtml [] = ""
configsToHtml (initial : configs) =  -- first elem alone
  "<article class=\"transition\">\n" ++
  configToSection initial "initial" ++
  "</article>\n\n" ++
  configsToHtmlLoop (initial : configs)


configsToHtmlLoop :: [Config] -> String
configsToHtmlLoop [] = ""
configsToHtmlLoop [_] = ""
configsToHtmlLoop (before : after : configs) =
  "<article class=\"transition\">\n" ++
  configToSection before "before" ++
  configToSection after "after" ++
  "</article>\n\n" ++
  configsToHtmlLoop (after : configs)


programs :: [(String, Stmt)]
programs = [
    ("assignment",  progAssigments)
  , ("min",         progMin)
  , ("factorial",   progFact)
  , ("euclid",      progEuclid)
  ]

pageFilename, templateFilename :: String
pageFilename = "page.html"
templateFilename = "page-template.html"

main :: IO()
main = do
  -- get program to evaluate
  args <- getArgs
  let programName = if null args then error "Error: must supply program name" else head args
  let program = case (lookup programName programs) of
                  Just p  -> p
                  Nothing -> error $ "Available programs: " ++ (", " `intercalate` map fst programs)

  -- get the evaluation
  let configs = executionConfigs program
  let configs' = configsToHtml configs

  -- open the page for template
  contents <- readFile templateFilename
  if null contents then error $ "Error: nothing in page template - " ++ templateFilename else do

    -- generate new page content
    let contents' = replaceConfigs contents configs'

    -- write them back
    writeFile pageFilename contents'  -- force lazy evaluation so the file can be written to
