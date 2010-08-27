{-# LANGUAGE ExistentialQuantification, ViewPatterns, 
        GeneralizedNewtypeDeriving #-}
module CombinatorCalculus 
    (Name, Combinator(takesArgs,applyArgs),
     Term(), Expr(),
     named, expr, isNormal, evalNormalOrder, (·))
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
      -add eval function that evaluates arguments fully:
          -applicative order: evaluates arguments to normal form before applying
      -'read' instance for expressions
-}
------------------------------------------------------------


 -- operator for composing combinator expressions. This is the Middle Dot, 
 -- unicode character 00B7. It is easily inserted in vim using the digraph: 
 --       Ctrl-K '.' 'M'
infixl 7 ·

    ---------------------
    -- TYPES AND CLASSES:
    ---------------------

                                         
 -- a Class for combinators. Its methods represent a combinator's behavior on 
 -- its arguments:
class (Show c)=> Combinator c where
    takesArgs :: c -> Int
    applyArgs :: c -> [Term] -> Expr
     -- HIDDEN METHOD:
    toTerm :: c -> Term
    toTerm = Term



    ----------------------


type Name = String

data Term = forall c. Combinator c => Term { unbox :: c }
          | SubExpr   { subExpr :: Expr }
          | NamedExpr { subExpr :: Expr,
                        name    :: String }

instance Show Term where
    show (NamedExpr _ n) = n
    show (SubExpr e)     = "("++ show e ++")"
    show (Term t)        = show t


instance Combinator Term where
    toTerm = id

    takesArgs (Term c) = takesArgs c
    takesArgs s        = takesArgs (subExpr s)

    applyArgs (Term c) = applyArgs c 
    applyArgs e        = applyArgs (subExpr e)

   
 

    ----------------------


newtype Expr = Expr { combSeq :: Seq Term }

instance Show Expr where
    show = unwords . map show . F.toList . combSeq

instance Combinator Expr where
    toTerm = SubExpr

    applyArgs e = appendTerms e . fromList 

    takesArgs (viewl.combSeq-> c:<cs) = 
        let argsNeeded = (takesArgs c) - (Data.Sequence.length cs)
         in if argsNeeded < 0  then 0  else argsNeeded



    --------------------
    -- HELPER FUNCTIONS:
    --------------------

 -- for composing Combinator expressions:
(·) :: (Combinator c1,Combinator c2)=> c1 -> c2 -> Expr
c1 · (toTerm->t2) = case toTerm c1 of
                         t1@(Term _)  -> Expr (singleton t1 |> t2)
                         (subExpr->e) -> e `appendTerms` singleton t2
    

 -- Allows for re-defining the Show instance of some Expr, letting you treat the
 -- expression like any other Combinator:
named :: Expr -> Name -> Term
named = NamedExpr

 -- For making a singleton expression:
expr :: (Combinator c)=> c -> Expr
expr = Expr . singleton . toTerm

 -- NOT EXPORTED:
appendTerms :: Expr -> (Seq Term) -> Expr
appendTerms (Expr s) s' = Expr ( s >< s') 


    ------------------------
    -- EVALUATION FUNCTIONS:
    ------------------------


 -- is the Expression in a state where it cannot be evaluated further?:
isNormal :: Expr -> Bool
isNormal = norm . SubExpr
    where norm (Term c) = takesArgs c > 0
          norm e = (takesArgs e > 0)  && 
                   (F.all norm $ combSeq $ subExpr e)




 -- evaluate leftmost-outermost redex until top level expression is in Normal
 -- Form:
evalNormalOrder :: Expr -> [Expr]
evalNormalOrder e@(viewl.combSeq-> c:<cs)  
    | argsReq > argsAvail = [e]
    | otherwise = e : evalNormalOrder (cEvaled `appendTerms` csRest)
            -- does combinator 'c' have enough arguments to evaluate?:
     where argsReq         = takesArgs c
           argsAvail        = Data.Sequence.length cs
            -- the transformed expression with remaining arguments:
           (csArgs',csRest) = Data.Sequence.splitAt argsReq cs
           cEvaled         = applyArgs c (F.toList csArgs')
           

 -- Evaluates arguments to functions fully, before applying combinator to args:
{-
evalApplicativeOrder :: Expr -> [Expr]
evalApplicativeOrder e@(viewl.combSeq-> c:<cs)  
    | argsReq > argsAvail = [e]
    | otherwise = e : evalNormalOrder (cEvaled `appendTerms` csRest)
           
     where argsReq   = takesArgs c
           argsAvail = Data.Sequence.length cs
           (csArgs',csRest) = Data.Sequence.splitAt argsReq cs
           cEvaled          = applyArgs c (F.toList csArgs')
-}
