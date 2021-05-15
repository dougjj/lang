module Program where

import Control.Monad.State
import Environment
import Language
import Control.Monad ( when )

type States m a = StateT Environment m a

doStatement :: Statement -> States IO ()
doStatement (Assignment str expr) = do
    num <- evalExpr expr
    modify $ defineVar str num

doStatement (IfThenElse expr s1 s2) = do
    num <- evalExpr expr
    if num /= 0
        then doStatement s1
        else doStatement s2

doStatement (Print expr) = do
    num <- evalExpr expr
    lift $ print num

doStatement (Input str) = do
    input <- lift readLn
    modify $ defineVar str input

doStatement (Semicolon s1 s2) = doStatement s1 >> doStatement s2

doStatement (FunctionAssignment str args body) = do
    modify $ defineFunc str (args, body)

doStatement s@(WhileDo expr body) = do
    val <- evalExpr expr
    when (val /= 0) $ doStatement body >> doStatement s

boolToInt :: Bool -> Number
boolToInt b = if b then 1 else 0

evalExpr :: Expression -> States IO Number
evalExpr (Number x) = return x
evalExpr (Variable str) = gets (lookupVar str)
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

evalExpr (Function str args) = do
    vals <- mapM evalExpr args
    (vars, body) <- gets $ lookupFunc str
    modify $ newBindEnv vars vals
    -- newEnv <- get 
    -- lift $ print newEnv 
    doStatement body
    modify safeParent
    return 1