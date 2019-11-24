{-|
Module: Chups
Description: Chups language for Assignment 2
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

The assignment handout can be found at
https://www.cs.toronto.edu/~david/csc324/assignments/a2/handout.html
-}
module Chups
    (
    -- Task 1 (CPS transform, stateless)
      cpsTransformProg
    , cpsTransform
    -- Tasks 2 & 3 (CPS transform, stateful; manipulating control flow)
    , cpsTransformProgS
    , cpsTransformS
    )
where

import qualified Data.List as List
import qualified Control.Monad.State as State
import ChupsTypes (Prog(..), Binding(..), Expr(..))

-------------------------------------------------------------------------------
-- |
-- * Task 1: Implementing CPS transformation (stateless)
-------------------------------------------------------------------------------

-- | The main transformation function. This should take a Chups program in the
-- core language, and return a CPS-transformed version of this program.
-- Remember to use pass the `_id` continuation for top-level expressions.
cpsTransformProg :: Prog -> Prog
cpsTransformProg (Prog bindings expr) =
    let
        contContext = (Identifier "_id")
        bindingsTrans = foldl
            (\acc binding -> acc ++ [cpsTransformBinding binding contContext]) 
            []
            bindings
        exprTrans = cpsTransform expr contContext
    in
        Prog bindingsTrans exprTrans

cpsTransformBinding :: Binding -> Expr -> Binding
cpsTransformBinding (Binding identifier expr) k =
    Binding identifier (cpsTransform expr k)

-- | The main transformation function, which takes two Expr values:
-- the first is the expression to transform, and the second is (an Expr representation of)
-- a unary function representing the continuation to apply to the expression.
-- The resulting expression should be semantically equivalent to
-- calling the continuation on the expression.
cpsTransform :: Expr -> Expr -> Expr 
-- Implementation of cpsTransform for core expressions
-- Literals
cpsTransform (IntLiteral x) k = Call k [(IntLiteral x)]
cpsTransform (BoolLiteral x) k = Call k [(BoolLiteral x)]
cpsTransform (Identifier x) k = Call k [(Identifier x)]

-- Function definitions
cpsTransform (Lambda args body) k = 
    let identifier = "_k"
        cpsT = Lambda (args ++ [identifier]) (cpsTransform body (Identifier identifier))
    in
        Call k [cpsT]

-- Function calls
cpsTransform (Call func args) k = 
    if (allSubexpressionsAtomic ([func] ++ args))
        then Call func (args ++ [k])
        else handleNonAtomic (Call func args) k []

-- If confitions
cpsTransform (If cond bodyTrue bodyFalse) k =
    let bodyTrueTrans = cpsTransform bodyTrue k
        bodyFalseTrans = cpsTransform bodyFalse k
        condTrans = if (checkAtomic cond)
            then cond
            else cpsTransform cond k
    in
        If condTrans bodyTrueTrans bodyFalseTrans


-- Remember that for Task 1, you only need to handle the core Chups expression types.
-- You can leave this pattern-match line to prevent an "unmatched pattern" compiler warning.
cpsTransform _ _ =
    error "For Task 1, you do not need to handle other expression types."

-------------------------------------------------------------------------------
-- |
-- * Task 2: Implementing CPS transformation (stateful)
-------------------------------------------------------------------------------
-- | This is similar to cpsTransformProg, except that it uses a stateful
-- version of cpsTransform to generate unique names and avoid name collisions.
cpsTransformProgS :: Prog -> Prog
cpsTransformProgS (Prog bindings expr) =
    let
        contContext = (Identifier "_id")
        bindingsTrans = foldl
            (\acc binding -> acc ++ [cpsTransformBindingS binding contContext]) 
            []
            bindings
        exprTrans = State.evalState (cpsTransformS expr contContext) 0
    in
        Prog bindingsTrans exprTrans

cpsTransformBindingS :: Binding -> Expr -> Binding
cpsTransformBindingS (Binding identifier expr) k =
    Binding identifier (State.evalState (cpsTransformS expr k) 0)

-- | Stateful version of cpsTransform, which uses its Integer state as a counter
-- to generate fresh variable names. See assignment handout for details.
cpsTransformS :: Expr -> Expr -> State.State Integer Expr
cpsTransformS (IntLiteral x) k = return $ Call k [(IntLiteral x)]
cpsTransformS (BoolLiteral x) k = return $ Call k [(BoolLiteral x)]
cpsTransformS (Identifier x) k = return $ Call k [(Identifier x)]

-- Function definitions
cpsTransformS (Lambda args body) k = do
    counter <- State.get
    increment
    let identifier = "_k" ++ show counter
    bodyTrans <- cpsTransformS body (Identifier identifier)
    let cpsT = Lambda (args ++ [identifier]) bodyTrans
    return $ Call k [cpsT]

-- Function calls
cpsTransformS (Call func args) k = do
    if (allSubexpressionsAtomic ([func] ++ args))
        then return $ Call func (args ++ [k])
        else handleNonAtomicS (Call func args) k []

-- If confitions
cpsTransformS (If cond bodyTrue bodyFalse) k = do
    bodyTrueTrans <- cpsTransformS bodyTrue k
    bodyFalseTrans <- cpsTransformS bodyFalse k
    if (checkAtomic cond)
        then 
            return $ If cond bodyTrueTrans bodyFalseTrans
        else do
            condTrans <- cpsTransformS cond k
            return $ If condTrans bodyTrueTrans bodyFalseTrans
-------------------------------------------------------------------------------
-- |
-- * HELPERS
-------------------------------------------------------------------------------
-- Helper function to determine whether or not all arguments are literals/identifiers
allSubexpressionsAtomic :: [Expr] -> Bool
allSubexpressionsAtomic expr = all (checkAtomic) expr

-- Returns whether or not the given expression is atomic.
checkAtomic :: Expr -> Bool
checkAtomic (IntLiteral x) = True
checkAtomic (BoolLiteral x) = True
checkAtomic (Identifier x) = True
checkAtomic _ = False

-- Helper for handling case where function call is non atomic
handleNonAtomic :: Expr -> Expr -> [Expr] ->Expr
-- case where 'f' subexpression is atomic
handleNonAtomic (Call (Identifier f) (x:xs)) k seen =
    if (checkAtomic x)
        then
            handleNonAtomic (Call (Identifier f) xs) k (seen ++ [x])
        else
            let newArgs = seen ++ [Identifier "_v"] ++ xs
                bodyTrans = cpsTransform (Call (Identifier f) newArgs) k
                newLam = Lambda ["_v"] bodyTrans
            in
                cpsTransform x newLam

-- 'f' subexpression is non atomic
handleNonAtomic (Call func args) k seen =
    let bodyTrans = cpsTransform (Call (Identifier "_v") args) k
        newLam = Lambda ["_v"] bodyTrans
    in
        cpsTransform func newLam

-- Helper for handling case where function call is non atomic
-- using state
handleNonAtomicS :: Expr -> Expr -> [Expr] -> State.State Integer Expr
-- case where 'f' subexpression is atomic
handleNonAtomicS (Call (Identifier f) (x:xs)) k seen = do
    if (checkAtomic x)
        then
            handleNonAtomicS (Call (Identifier f) xs) k (seen ++ [x])
        else do
            counter <- State.get
            increment
            let identifier = "_v" ++ show counter
            let newArgs = seen ++ [Identifier identifier] ++ xs
            bodyTrans <- cpsTransformS (Call (Identifier f) newArgs) k
            let newLam = Lambda [identifier] bodyTrans
            cpsTransformS x newLam

-- 'f' subexpression is non atomic
handleNonAtomicS (Call func args) k seen = do
    counter <- State.get
    increment
    let identifier = "_v" ++ show counter
    bodyTrans <- cpsTransformS (Call (Identifier identifier) args) k
    let newLam = Lambda [identifier] bodyTrans
    cpsTransformS func newLam

increment :: State.State Integer ()
increment = do
    curr <- State.get
    State.put (curr + 1)