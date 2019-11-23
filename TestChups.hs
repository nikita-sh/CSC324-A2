{-|
Module: TestChups
Description: Sample tests for Assignment 2
Copyright: (c) University of Toronto, 2019
               CSC324 Principles of Programming Languages, Fall 2019

Warning: as usual, these sample tests are very incomplete, and are meant to
give you a sense of the test structure we'll use, but NOT to verify the
complete correctness of your work on this assignment! Please add your own
tests here.
-}
import qualified Data.List as List
import qualified Control.Monad.State as State
import Control.Monad (mapM_)
import Test.QuickCheck (quickCheck)

import ChupsTypes (Prog(..), Binding(..), Expr(..))
import Chups
    (
    -- Task 1

      cpsTransformProg
    , cpsTransform

    -- Tasks 2 & 3

    , cpsTransformProgS
    , cpsTransformS
    )

-------------------------------------------------------------------------------
-- |
-- * Sample tests for Task 1
-------------------------------------------------------------------------------
--
-- Note: these tests use a dummy continuation of an identifier `cont`.
-- Remember that you aren't checking whether names are bound or not for
-- the CPS transformations.

prop_intLiteral :: Bool
prop_intLiteral = cpsTransform (IntLiteral 10) (Identifier "cont")
    == Call (Identifier "cont") [IntLiteral 10]

prop_identifier :: Bool
prop_identifier = cpsTransform (Identifier "x") (Identifier "cont")
    == Call (Identifier "cont") [Identifier "x"]

prop_thunk :: Bool
prop_thunk =
    cpsTransform (Lambda [] (IntLiteral 10)) (Identifier "cont") ==
    -- Note the exact parameter name for the continuation.
    Call
        (Identifier "cont")
        [Lambda ["_k"] (Call (Identifier "_k") [IntLiteral 10])]

prop_callAllAtomic :: Bool
prop_callAllAtomic =
    cpsTransform
            (Call (Identifier "f") [IntLiteral 1, IntLiteral 2])
            (Identifier "cont")
        == Call (Identifier "f") [IntLiteral 1, IntLiteral 2, Identifier "cont"]

-- | Test for ((lambda (x) (* x 200)) 10)
-- Note: you can always use `show` to see a Racket-compatible syntax for these expressions.
prop_callFuncNonAtomic :: Bool
prop_callFuncNonAtomic =
    cpsTransform
            (Call
                (Lambda
                    ["x"]
                    (Call (Identifier "cps:*") [Identifier "x", IntLiteral 200])
                )
                [IntLiteral 10]
            )
            (Identifier "cont")
        == Call
               (Lambda
                   ["_v"]
                   (Call (Identifier "_v") [IntLiteral 10, Identifier "cont"])
               )
               [ Lambda
                     ["x", "_k"]
                     (Call
                         (Identifier "cps:*")
                         [Identifier "x", IntLiteral 200, Identifier "_k"]
                     )
               ]

-- | Test for (+ (* 10 20) 30 (* 40 50)).
prop_callNested :: Bool
prop_callNested =
    cpsTransform
            (Call
                (Identifier "cps:+")
                [ Call (Identifier "cps:*") [IntLiteral 10, IntLiteral 20]
                , IntLiteral 30
                , Call (Identifier "cps:*") [IntLiteral 40, IntLiteral 50]
                ]
            )
            (Identifier "cont")
        ==
           -- Note that there are two occurrences of _v, which illustrates name collision.
           -- This is *expected* for cpsTransform in Task 1, but you'll address this in Task 2.
           Call
               (Identifier "cps:*")
               [ IntLiteral 10
               , IntLiteral 20
               , Lambda
                   ["_v"]
                   (Call
                       (Identifier "cps:*")
                       [ IntLiteral 40
                       , IntLiteral 50
                       , Lambda
                           ["_v"]
                           (Call
                               (Identifier "cps:+")
                               [ Identifier "_v"
                               , IntLiteral 30
                               , Identifier "_v"
                               , Identifier "cont"
                               ]
                           )
                       ]
                   )
               ]

-- | Test to demonstrate a CPS transformation on a full Chups program.
-- The *identity function* is passed as the continuation to every top-level expression.
prop_prog :: Bool
prop_prog =
    cpsTransformProg
            (Prog
                [ Binding
                    "f"
                    (Lambda
                        ["x"]
                        (Call
                            (Identifier "cps:*")
                            [Identifier "x", IntLiteral 10]
                        )
                    )
                , Binding "a" (IntLiteral 200)
                ]
                (Call (Identifier "f") [Identifier "a"])
            )
        == (Prog
               [ Binding
                   "f"
                   (Call
                       (Identifier "_id")
                       [ Lambda
                             ["x", "_k"]
                             (Call
                                 (Identifier "cps:*")
                                 [ Identifier "x"
                                 , IntLiteral 10
                                 , Identifier "_k"
                                 ]
                             )
                       ]
                   )
               , Binding "a" (Call (Identifier "_id") [IntLiteral 200])
               ]
               (Call (Identifier "f") [Identifier "a", Identifier "_id"])
           )


-------------------------------------------------------------------------------
-- |
-- * Sample tests for Task 2
-------------------------------------------------------------------------------

-- | Test for ((lambda (x) (* x 200)) 10), stateful version.
prop_callFuncNonAtomicS :: Bool
prop_callFuncNonAtomicS =
    State.evalState
            (cpsTransformS
                (Call
                    (Lambda
                        ["x"]
                        (Call
                            (Identifier "cps:*")
                            [Identifier "x", IntLiteral 200]
                        )
                    )
                    [IntLiteral 10]
                )
                (Identifier "cont")
            )
            0
        == Call
               (Lambda
                   ["_v0"]
                   (Call (Identifier "_v0") [IntLiteral 10, Identifier "cont"])
               )
               [ Lambda
                     ["x", "_k1"]
                     (Call
                         (Identifier "cps:*")
                         [Identifier "x", IntLiteral 200, Identifier "_k1"]
                     )
               ]

-- | Test for (+ (* 10 20) 30 (* 40 50)), stateful version.
prop_callNestedS :: Bool
prop_callNestedS =
    State.evalState
            (cpsTransformS
                (Call
                    (Identifier "cps:+")
                    [ Call (Identifier "cps:*") [IntLiteral 10, IntLiteral 20]
                    , IntLiteral 30
                    , Call (Identifier "cps:*") [IntLiteral 40, IntLiteral 50]
                    ]
                )
                (Identifier "cont")
            )
            0
        ==
           -- Note that there are two occurrences of _v, which illustrates name collision.
           -- This is *expected* for cpsTransform in Task 1, but you'll address this in Task 2.
           Call
               (Identifier "cps:*")
               [ IntLiteral 10
               , IntLiteral 20
               , Lambda
                   ["_v0"]
                   (Call
                       (Identifier "cps:*")
                       [ IntLiteral 40
                       , IntLiteral 50
                       , Lambda
                           ["_v1"]
                           (Call
                               (Identifier "cps:+")
                               [ Identifier "_v0"
                               , IntLiteral 30
                               , Identifier "_v1"
                               , Identifier "cont"
                               ]
                           )
                       ]
                   )
               ]


main :: IO ()
main = mapM_ quickCheck
    [
    -- Task 1 tests
      prop_intLiteral
    , prop_identifier
    , prop_thunk
    , prop_callAllAtomic
    , prop_callFuncNonAtomic
    , prop_callNested
    , prop_prog
    -- Task 2 tests

    , prop_callFuncNonAtomicS
    , prop_callNestedS
    ]
