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
cpsTransform (IntLiteral x) k = k x
cpsTransform (BoolLiteral x) k = k x
cpsTransform (Identifier x) k = k x 

-- Function definitions
cpsTransform (Lambda args body) k = undefined 

-- Function calls
cpsTransform (Call func args) k = undefined 

-- If confitions
cpsTransform (If cond bodyTrue bodyFalse) k = undefined

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
