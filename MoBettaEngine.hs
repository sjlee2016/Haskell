module MoBettaEngine where

{--
The main job of this module is to provide a translation of the abstract syntax tree of a program into an executable computation.

A computation here involves two main features:
 - IO for interacting with the user
 - A state that represents storage of integers in variables.

The state allows a program to interpret expressions like "x + 1" and to update
variables, so that assignment statements are possible.
We keep the state in the form of a Data.HashMap. This is essentially a Haskell version of the Python notion of a dictionary.
Because our language does not support function definitions and recursion, the state does not need to involve a stack.
--}

import System.IO
--import Data.Hashable
import qualified Data.HashMap as HM-- easy lookup and update of variables
import Control.Monad.State
import Control.Applicative
import Data.Maybe (fromMaybe) -- using fromMaybe to simplify some code

import MoBettaAST

-- Env is a "dictionary" of (variable, int) pairs. We use this to model storing values in variables. The HM.Map type constructor supports lookup and update of variables.
type Env = HM.Map String Integer

-- We need an empty environment at the beginning of a computation when no variables have yet been used.
emptyEnv :: Env
emptyEnv = HM.fromList []

-- Declare a "computation" to combine IO and ability to deal with an environment.
-- StateT combines a state type (in this case Env) and an existing monad (IO) to produce a monad in which a computation is essentially an IO computation that can access and modify the state.

type Computation t = StateT Env IO t

-- For clarity, we declare
--     Action : a computation that does not compute a value.
--     IntCalc : a computation that produces an Integer
--     BoolCalc : a computation that produces a Boolean

type Action = Computation ()
type IntCalc = Computation Integer
type BoolCalc = Computation Bool

-- Now define by cases the main translation from abstract syntax to computation (helper functions will follow)
-- For uniformity, I've written all of these in the same way, by calling
-- lower-level functions that define the semantics in terms of sub-computations.
-- Roughly, each different kind of statement translates to a different kind of "action".

statementAction :: Statement -> Action
statementAction (Print e) = printAction (intCalc e) -- display result of calculating e
statementAction (Msg s) = msgAction s -- display string s
statementAction (Read v) =  readAction v -- read in a value for v
statementAction (If b s1 s2) =
  ifAction (boolCalc b) (statementAction s1) (statementAction s2)
                        -- Calculate b, then decide which computation to do
statementAction (While b s) = whileAction (boolCalc b) (statementAction s)
                        -- compute "while b s"
statementAction (Assign v e) = assignAction v (intCalc e)
                        -- compute e, and assign to v
statementAction (Block ls) = blockAction $ map statementAction ls
                        -- compute a sequence of individual computations
                        -- by translating each into a computation

makeProgram ls = blockAction $ map statementAction ls

{---------------------------------------------------------------------------
Some helpers to manipulate the state and to access IO.
----------------------------------------------------------------------------}

-- This turns an IO  into a Computation
-- We need this to perform IO within the Computation monad.
-- In a Computation block, write "doIO print" instead of "print", etc.
doIO :: IO a -> Computation a
doIO = lift

-- Helper to update the store, modelling assignment to a variable
-- "modify" is supplied by StateT. It works be taking the given state (an Env)
-- and applying the given function. In this case, "HM.insert name val"
-- is a function that inserts the pair (name,val) into an environment, replacing any old (name,x) pair it that existed.
updateEnv :: String -> Integer -> Computation ()
updateEnv name val = modify $ HM.insert name val

-- Helper to get the value of a variable from the store
--  return Nothing if variable is not present
-- "gets" refers to "get state". In our setting, it is a Computation that extracts the state (an Env) and applies the given function.
-- So this uses "HM.lookup name" on the current environment.
-- HM.lookup can fail if the identifier we are trying to retrieve does not exist. So "val" is a "Maybe Int" -- "fromMaybe" is a simple way to deal with Maybe failures.
retrieveEnv :: String -> Computation Integer
retrieveEnv name = do
  val <- gets $ HM.lookup name
  return $ fromMaybe (varNotFound name) val
  where
    varNotFound name = error $ "Identifier \"" ++ name ++ "\" not defined."

{--------------------------------------------------------------------------
Interpretations of individual statement types

Now we define the semantics of each type of action.
---------------------------------------------------------------------------}

-- Read and store an integer in a variable
readAction :: String -> Action
readAction v = do
  x <- doIO getInt
  updateEnv v x
  where
    getInt = do
      inp <- getLine
      return $ read inp

-- Display a string
msgAction :: String -> Action
msgAction s = doIO $ putStr s

-- Display result of computing an integer
printAction :: IntCalc -> Action
printAction intCalc = do
  n <- intCalc -- execute given integer calculation to obtain an Integer
  doIO $ putStr $ show n -- display it

-- Compute an integer, then store it
assignAction :: String -> IntCalc -> Action
assignAction v intCalc = do
  n <- intCalc -- calculate the right-hand side of an assignment
  updateEnv v n -- store the result

-- Compute a boolean, use it to decide which computation to do.
ifAction :: BoolCalc -> Action -> Action -> Action
ifAction boolCalc action1 action2 = do
  cond <- boolCalc -- calculate the test condition
  if cond then     -- decide what to do
    action1
  else
    action2

whileAction :: BoolCalc -> Action -> Action
whileAction boolCalc action = do
  cond <- boolCalc -- calculate the test
  when cond loop   -- do the body of the loop
  where
    loop = do
      action
      whileAction boolCalc action

-- Do a list of actions sequentially.
-- NB. There is a combinator for this, but I've written it out for clarity.
blockAction :: [Action] -> Action
blockAction [] = return ()
blockAction (a:ls) = do
  a
  blockAction ls

{--------------------------------------------------------------------------
Interpretations of integer and boolean expressions
---------------------------------------------------------------------------}

-- Lookup tables for the various arithmetic and Boolean operators.
--  These associate AST operators with their semantics.
-- I have written things this way to make integer and boolean calculations
-- easier to code.

aBinOps =
  [ (Add, (+))
  , (Sub, (-))
  , (Mul, (*))
  , (Div, div)
  , (Mod, mod)]

aUnOps =  [(Neg, negate)]

bBinOps =
  [ (And, (&&))
  , (Or, (||))]

bUnOps =  [(Not, not)]

relnOps =
  [ (Greater, (>))
  , (GreaterEqual, (>=))
  , (Less, (<))
  , (LessEqual, (<=))
  , (Equal, (==))
  , (NEqual, (/=))]

-- Find the actual operation associated with a constructor.
-- N.B. lookup returns a (Maybe ...).
-- But the only way it would return Nothing
--   is if something impossible has happened (the parser somehow produced an
--   impossible AST).
-- So it is safe to unwrap a result of the form (Just ...)
-- The combination of fromMaybe with (error ...) does that
opLookup :: Eq const => const -> [(const, sem)] -> sem
opLookup op opTable =
  fromMaybe (error "Undefined operator. Should never happen.")
            (lookup op opTable)

-- This defines the translation of a BExpr into a computation of Booleans
boolCalc :: BExpr -> BoolCalc
boolCalc (BoolConst b) = return b
boolCalc (Reln cOp expr1 expr2)
  = liftA2 (opLookup cOp relnOps) (intCalc expr1) (intCalc expr2)
boolCalc (BBin op expr1 expr2)
  = liftA2 (opLookup op bBinOps) (boolCalc expr1) (boolCalc expr2)
boolCalc (BUn op expr)
  = fmap (opLookup op bUnOps) (boolCalc expr)

intCalc :: AExpr -> IntCalc
intCalc (Var v) = retrieveEnv v
intCalc (IntConst val) = return val
intCalc (ABin op expr1 expr2)
  = liftA2 (opLookup op aBinOps) (intCalc expr1) (intCalc expr2)
intCalc (AUn op expr)
  = fmap (opLookup op aUnOps) (intCalc expr)
