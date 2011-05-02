module Main where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances hiding (Parser, input)
import System.Console.Haskeline
import System.Environment (getArgs)

type Parser a = P (Str Char String LineColPos) a

main :: IO ()
main = do
  args <- getArgs
  if null args then interactive 
               else putStrLn $ run timeExpr (unwords args)

interactive :: IO ()
interactive = runInputT defaultSettings loop
  where 
   loop :: InputT IO ()
   loop = do minput <- getInputLine "> "
             case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do outputStrLn $ run timeExpr input
                                loop


run :: Parser String -> String -> String
run p inp = do  let (a, errors) =  parse ( (,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) inp)
                if null errors then  a
                               else  "Error in expression"

timeExpr :: Parser String
timeExpr = format <$> expr
 where format :: Double -> String
       format x = let minutes :: Integer
                      seconds :: Integer
                      (minutes,secondsMultiplier) = properFraction x
                      seconds = round $ secondsMultiplier * 60
                      showSeconds s | s < 10    = "0" ++ show s
                                    | otherwise = show s
                  in  show minutes ++ ":" ++ showSeconds seconds

expr :: Parser Double
expr = foldr pChainl ( pDouble <|> pTime <|>pParens expr) (map same_prio operators) 
 where
  operators       = [[('+', (+)), ('-', (-))],  [('*' , (*))], [('/' , (/))]]
  same_prio  ops  = foldr (<|>) empty [ op <$ lexeme (pSym c) | (c, op) <- ops]

pTime :: Parser Double
pTime = lexeme pRawMinuteTime <|> lexeme pRawHourTime

pRawMinuteTime :: Parser Double
pRawMinuteTime = makeTime <$> pIntegerRaw <* pSym ':' <*> pIntegerRaw <?> "min:sec"
 where makeTime x y = x + (y / 60.0)

pRawHourTime :: Parser Double
pRawHourTime = makeTime <$> pIntegerRaw <* pSym ':' <*> pIntegerRaw <* pSym ':' <*> pIntegerRaw <?> "hour:min:sec"
 where makeTime x y z = x * 60 + y + z / 60.0
