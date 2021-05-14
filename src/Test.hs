module Test where 

import Parsing 
import Evaluator 
import Language 
import Text.Parsec (parse) 
import Control.Monad.Trans
import qualified Data.Map as M
-- import Control.Monad.State (runStateT) 
import Program 
import Environment 
import Control.Monad.State

runner :: States IO () 
runner = do 
    line <- lift getLine 
    doStatement $ parseBlock line 
    env <- get 
    lift $ print env

    runner 

main = runStateT runner emptyEnv 