module Evaluator where 

import Language 

import Data.Functor.Identity ( Identity )
import qualified Data.Map as M
import Control.Monad.State as S
import Control.Monad.Identity
import Text.Read ( readMaybe ) 


type SymbolTable = M.Map String Number
type IOEvaluator a = StateT SymbolTable IO a
type Evaluator a = StateT SymbolTable Identity a 


-- addFunction :: String -> Statement -> Evaluator () 
-- addFunction name body = do 
--     table <- get 
--     put $ M.insert name body 

lookUp :: String -> Evaluator Number
lookUp str = do
    table <- get
    case M.lookup str table of
        Just value -> return value
        Nothing -> error $ "undefined variable" ++ str

addSymbol :: String -> Number -> Evaluator ()
addSymbol str val = do
    table <- get
    put $ M.insert str val table

iolift :: Evaluator a -> IOEvaluator a
iolift = mapStateT (return . runIdentity)

boolToInt b = if b then 1 else 0


evalStmt :: Statement -> IOEvaluator ()
evalStmt (Assignment name expr) = iolift $ do
    val <- evalExpr expr
    addSymbol name val

evalStmt (IfThenElse e s1 s2) = do
    val <- iolift $ evalExpr e
    if val /= 0 then evalStmt s1 else evalStmt s2

evalStmt (Print expr) = do
    val <- iolift $ evalExpr expr
    lift $ print val

evalStmt (Input name) = do
    input <- lift getLine
    case readMaybe input of
        Just num -> iolift $ addSymbol name num 
        Nothing -> lift (print "num only!") >> evalStmt (Input name) 

evalStmt (Semicolon s1 s2) = evalStmt s1 >> evalStmt s2 

-- evalStmt (FunctionAssignment name args stmt) = do 

evalExpr :: Expression -> Evaluator Number
evalExpr (Number x) = return x
evalExpr (Variable name) = do
    lookUp name
evalExpr (BinaryOp op x y) = do
    xx <- evalExpr x
    yy <- evalExpr y
    return $ case op of
        Plus -> xx + yy
        Minus -> xx - yy
        Times -> xx * yy
        Divide -> div xx yy

        Lt -> boolToInt $ xx < yy
        Gt -> boolToInt $ xx > yy
        Leq -> boolToInt $ xx <= yy
        Geq -> boolToInt $ xx >= yy

        Equals -> boolToInt $ xx == yy
        NotEquals -> boolToInt $ xx /= yy

evalExpr (UnaryOp op x) = do
    xx <- evalExpr x
    return $ case op of
        UnaryMinus -> - xx
        Not -> if xx == 0 then 1 else 0 