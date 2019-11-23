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
cpsTransformProg _ = undefined

-- | The main transformation function, which takes two Expr values:
-- the first is the expression to transform, and the second is (an Expr representation of)
-- a unary function representing the continuation to apply to the expression.
-- The resulting expression should be semantically equivalent to
-- calling the continuation on the expression.
cpsTransform :: Expr -> Expr -> Expr 
-- Implementation of cpsTransform for core expressions
-- Literals
cpsTransform (IntLiteral x) k = (Call k [(IntLiteral x)])
cpsTransform (BoolLiteral x) k = (Call k [(BoolLiteral x)])
cpsTransform (Identifier x) k = (Call k [(Identifier x)])

-- Function definitions
cpsTransform (Lambda args body) k = 
    let cpsT = (Lambda (args ++ ["_k"]) (cpsTransform body (Identifier "_k")))
    in
        (Call k [cpsT])

-- Function calls
cpsTransform (Call func args) k = 
    if (checkLiteralOrAtomic ([func] ++ args))
        then (Call func (args ++ [k]))
        else handleNonAtomic (Call func args) k []

-- If confitions
cpsTransform (If cond bodyTrue bodyFalse) k = 
    if (checkNonAtomic cond)
        then 
            let condTrans = cpsTransform cond k 
                bodyTrueTrans = cpsTransform bodyTrue k 
                bodyFalseTrans = cpsTransform bodyFalse k
            in 
                (If cond bodyTrueTrans bodyFalseTrans)
        else 
            let bodyTrueTrans = cpsTransform bodyTrue k
                bodyFalseTrans = cpsTransform bodyFalse k
            in
                (If cond bodyTrueTrans bodyFalseTrans)
                

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
cpsTransformProgS _ = undefined

-- | Stateful version of cpsTransform, which uses its Integer state as a counter
-- to generate fresh variable names. See assignment handout for details.
cpsTransformS :: Expr -> Expr -> State.State Integer Expr
cpsTransformS _ _ = undefined

-------------------------------------------------------------------------------
-- |
-- * HELPERS
-------------------------------------------------------------------------------
-- Helper function to determine whether or not all arguments are literals/identifiers
checkLiteralOrAtomic :: [Expr] -> Bool
checkLiteralOrAtomic expr = 
    foldl checkLiteralOrAtomicUpdate True expr

-- Update function for previous helper
checkLiteralOrAtomicUpdate :: Bool -> Expr -> Bool
checkLiteralOrAtomicUpdate prev (IntLiteral x) = 
    if prev
        then True
        else False

checkLiteralOrAtomicUpdate prev (BoolLiteral x) = 
    if prev 
        then True
        else False

checkLiteralOrAtomicUpdate prev (Identifier x) = 
    if prev 
        then True
        else False

checkLiteralOrAtomicUpdate prev new = False

-- Helper for handling case where function call is non atomic
handleNonAtomic :: Expr -> Expr -> [Expr] ->Expr
-- case where 'f' subexpression is atomic
handleNonAtomic (Call (Identifier f) args) k seen = 
    if (checkNonAtomic (head args))
        then 
            let newArgs = seen ++ [Identifier "_v"] ++ (tail args)
                bodyTrans = cpsTransform (Call (Identifier f) newArgs) k 
                newLam = Lambda ["_v"] bodyTrans 
            in 
                cpsTransform (head args) newLam 
        else 
            handleNonAtomic (Call (Identifier f) (tail args)) k (seen ++ [(head args)])

-- 'f' subexpression is non atomic
handleNonAtomic (Call func args) k seen = 
    let bodyTrans = cpsTransform (Call (Identifier "_v") args) k
        newLam = Lambda ["_v"] bodyTrans
    in
        cpsTransform func newLam

-- Returns whether or not the given expression is atomic.
checkNonAtomic :: Expr -> Bool
checkNonAtomic (Identifier x) = False 
checkNonAtomic (BoolLiteral x) = False 
checkNonAtomic (IntLiteral x) = False
checkNonAtomic x = True
