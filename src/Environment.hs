module Environment where

import Language
import Data.Map as M

import Control.Monad.Identity
import Control.Monad.State


data Environment = Environment {
    parentEnv :: Maybe Environment
    , variables :: M.Map String Number
    , functions :: M.Map String ([String], Statement)
} deriving (Show, Eq)

lookupVar :: String -> Environment -> Number
lookupVar str env = case M.lookup str (variables env) of 
        Just n -> n 
        Nothing -> case parentEnv env of 
            Nothing -> error $ "no var binding for " ++ str ++ show env 
            Just parent -> lookupVar str parent

lookupFunc :: String -> Environment -> ([String], Statement)
lookupFunc str env = case M.lookup str (functions env) of 
    Just f -> f
    Nothing -> case parentEnv env of 
        Nothing -> error $ "no func binding for " ++ str
        Just parent -> lookupFunc str parent

newBindEnv :: [String] -> [Number] -> Environment -> Environment 
newBindEnv strs nums env = Environment {
    parentEnv = Just env 
    , variables = M.fromList $ zip strs nums 
    , functions = M.empty 
}

freshEnv :: Environment -> Environment
freshEnv env = Environment {
    parentEnv = Just env
    , variables = M.empty
    , functions = M.empty
} 

emptyEnv :: Environment
emptyEnv = Environment {
    parentEnv = Nothing
    , variables = M.empty
    , functions = M.empty 
}

safeParent :: Environment -> Environment
safeParent env = case parentEnv env of
    Nothing -> env
    Just parent -> parent

type EvaluatorE a = StateT Environment Identity a

defineVar :: String -> Number -> Environment -> Environment
defineVar name val env = env { variables = M.insert name val (variables env)}

defineFunc :: String -> ([String], Statement) ->  Environment -> Environment
-- defineFunc name body (Environment parent vars funcs) = Environment
--     where 
--         newFuncs = M.insert name body funcs 
--         newEnv = Environment parent vars newFuncs

defineFunc name body env = env{ functions = M.insert name body (functions env)}

-- lookUpFunc :: String -> 

addFunction :: Statement -> EvaluatorE ()
addFunction (FunctionAssignment name args body) = do
    modify $ defineFunc name (args, body)

bindVariables :: [String] -> [Number] -> M.Map String Number
bindVariables strs nums = M.fromList (zip strs nums)
-- Functions are not first class here

modifyVariables :: (M.Map String Number -> M.Map String Number) -> Environment -> Environment
modifyVariables f env = env { variables = f (variables env) }

newEnv :: [String] -> [Number] -> Environment -> Environment
newEnv strs nums = modifyVariables (M.union (bindVariables strs nums))


-- evalExpr :: Expression  -> EvaluatorE Number
-- evalExpr (Function name args) = do
--     vals <- mapM evalExpr args
--     env <- get
--     case M.lookup name (functions env) of 
--         Nothing -> error "function not defined"
--         Just (strs, body) -> modify (newEnv strs vals) >> evalStmt body
--     return 1
