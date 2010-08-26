{-# LANGUAGE ExistentialQuantification, ViewPatterns, 
        GeneralizedNewtypeDeriving #-}
module CombinatorCalculus (Name,Combinator(..),Term(),CombinatorExpr(),
                            named,expr,isNormal,evalNormalOrder,(*.),(*:))
    where


import Data.Sequence
import qualified Data.Foldable as F


{-
 GOALS:
     1- don't force an evaluation strategy
     2- allow users to define new combinators without exposing implementation
     3- disallow creation of non-sensical combinator expressions, or possible 
        mis-use of the module
     4- hide existentials in type signatures
-}

{-
    TODO:
      -add (Combinator c,Combinator c1)=> c -> c1 -> CombinatorExpr
      -add combinators for all four Expr / c combos
      -add eval function that evaluates arguments fully:
          -applicative order: evaluates arguments to normal form before applying
      -'read' instance for expressions

  DEFINITELY TODO:
    -make an Expression class that would let us use · for composing all four 
      possibilities  :-)
-}
------------------------------------------------------------

-- an alternative character: the middle dot, easily inserted in vim using
-- Ctrl-K '.' 'M'
-- ·

 -- operators for composing combinator expressions. Some quieter Unicode symbols
 -- would be fun to look at but annoying to input:
infixl 7 *.
infixl 7 *:

    ---------------------
    -- TYPES AND CLASSES:
    ---------------------

                                         
class (Show c)=> Combinator c where
    takesArgs :: c -> Int
    applyArgs :: c -> [Term] -> CombinatorExpr


    ----------------------


type Name = String

data Term = forall c. Combinator c => Term { unbox :: c }
          | SubExpr   { subExpr :: CombinatorExpr }
          | NamedExpr { subExpr :: CombinatorExpr,
                        name    :: String }

instance Show Term where
    show (NamedExpr _ n) = n
    show (SubExpr e)     = "("++ show e ++")"
    show (Term t)        = show t


instance Combinator Term where
    takesArgs (Term c) = takesArgs c
    takesArgs (viewl.combSeq.subExpr -> c:<cs) = 
        let argsNeeded = (takesArgs c) - (Data.Sequence.length cs)
         in if argsNeeded < 0  
               then 0  
               else argsNeeded

    applyArgs (Term c) cs = applyArgs c cs
    applyArgs e cs = subExpr e `appendTerms` fromList cs 


    ----------------------


newtype CombinatorExpr = Expr { combSeq :: Seq Term }

instance Show CombinatorExpr where
    show = unwords . map show . F.toList . combSeq




    --------------------
    -- HELPER FUNCTIONS:
    --------------------



(*.) :: (Combinator c)=> CombinatorExpr -> c -> CombinatorExpr
(Expr s) *. c = Expr (s |> Term c)


(*:) :: CombinatorExpr -> CombinatorExpr -> CombinatorExpr
(Expr s) *: e = Expr (s |> SubExpr e)


named :: CombinatorExpr -> Name -> Term
named = NamedExpr

expr :: (Combinator c)=> c -> CombinatorExpr
expr = Expr . singleton . Term


 -- NOT EXPORTED:
appendTerms :: CombinatorExpr -> (Seq Term) -> CombinatorExpr
appendTerms (Expr s) s' = Expr ( s >< s') 


    ------------------------
    -- EVALUATION FUNCTIONS:
    ------------------------


 -- is the Expression in a state where it cannot be evaluated further?:
isNormal :: CombinatorExpr -> Bool
isNormal = norm . SubExpr
    where norm (Term c) = takesArgs c > 0
          norm e = (takesArgs e > 0)  && 
                   (F.all norm $ combSeq $ subExpr e)




 -- evaluate leftmost-outermost redex until top level expression is in Normal
 -- Form:
evalNormalOrder :: CombinatorExpr -> [CombinatorExpr]
evalNormalOrder e@(viewl.combSeq-> c:<cs)  
    | argsReq > argsAvail = [e]
    | otherwise = e : evalNormalOrder (cEvaled `appendTerms` csRest)
            -- does combinator 'c' have enough arguments to evaluate?:
     where argsReq   = takesArgs c
           argsAvail = Data.Sequence.length cs
            -- the transformed expression with remaining arguments:
           (csArgs',csRest) = Data.Sequence.splitAt argsReq cs
           cEvaled          = applyArgs c (F.toList csArgs')
           

