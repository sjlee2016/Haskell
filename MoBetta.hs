module Main where

import System.Environment
import System.IO -- needed for file handling
import Control.Monad.Trans.State.Lazy
import Text.Megaparsec.Error


import MoBettaParser -- construct abstract syntax trees from source code
import MoBettaEngine -- construct computations from abstract syntax trees

main = do
    args <- getArgs
    let fileName = head args
    putStrLn $ "MoBetta running file: " ++ fileName ++ " ...\n"
    handle <- openFile fileName ReadMode
          -- open file named by initial argument
    contents <- hGetContents handle  -- get entire contents of file lazily
    let pr = mbparse fileName contents
    case pr of
      Right prog -> evalStateT (makeProgram prog) emptyEnv
      Left err  -> putStrLn $ parseErrorPretty' contents err
    hClose handle  -- your mom taught you to clean up after yourself
