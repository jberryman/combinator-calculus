{-# LANGUAGE ExistentialQuantification, ViewPatterns, 
        GeneralizedNewtypeDeriving #-}
module CombinatorCalculus 
    ( 
      Combinator(takesArgs,applyArgs),
      Term(), Expr(),
      named, expr, isNormal, traceToNormal, evalToNormal, (·)
    ) where


import Data.Sequence as S
import qualified Data.Foldable as F


{-
 GOALS:
     1- don't force an evaluation strategy
     2- allow users to define new combinators without exposing implementation
     3- disallow creation of non-sensical combinator expressions, or possible 
        mis-use of the module
     4- hide existentials in type signatures
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
    
     -- HIDDEN METHOD: EXPORTED AS A FUNCTION:
     -- if a combinator takes 0 args, we assume it can be evaluated and we
     -- say it is not in Normal Form:
    isNormal :: c -> Bool
    isNormal = (> 0) . takesArgs
    
     -- HIDDEN METHOD: user defined combinators are placed in a black box:
    toTerm :: c -> Term
    toTerm = Term



    ----------------------


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

    isNormal (Term c) = takesArgs c > 0
    isNormal e        = isNormal (subExpr e)
   
 

    ----------------------


newtype Expr = Expr { combSeq :: Seq Term }

instance Show Expr where
    show = unwords . map show . F.toList . combSeq

instance Combinator Expr where
    toTerm = SubExpr

    applyArgs e = appendTerms e . fromList 

    takesArgs (viewl.combSeq-> c:<cs) = 
        let argsNeeded = (takesArgs c) - (S.length cs)
         in if argsNeeded < 0  then 0  else argsNeeded

    isNormal e@(Expr s) = (takesArgs e > 0) && (F.all isNormal s)



    --------------------
    -- HELPER FUNCTIONS:
    --------------------


 -- for composing Combinator expressions:
(·) :: (Combinator c1,Combinator c2)=> c1 -> c2 -> Expr
c1 · (toTerm->t2) = 
     case toTerm c1 of                                                                
          (SubExpr e) -> e `appendTerms` singleton t2                                 
           -- named expression or bare Term start a new sub-expression:               
          t1          -> Expr (singleton t1 |> t2)


 -- Allows for re-defining the Show instance of some Expr, letting you treat the
 -- expression like any other Combinator:
named :: Expr -> String -> Term
named = NamedExpr

 -- For making a singleton expression:
expr :: (Combinator c)=> c -> Expr
expr = Expr . singleton . toTerm




---- NOT EXPORTED:

 -- add a sequence of terms to end of expression:
appendTerms :: Expr -> Seq Term -> Expr
appendTerms (Expr s) s' = Expr ( s >< s') 

 -- for applying functions on inards of Expr type:
onSeq :: (Seq Term -> Seq Term) -> Expr -> Expr
onSeq f = Expr . f . combSeq

isTerm (Term _) = True
isTerm _        = False


    ------------------------
    -- EVALUATION FUNCTIONS:
    ------------------------
 

evalToNormal :: Expr -> Expr
evalToNormal = last . traceToNormal 


    -- EVALUATION HELPERS


 -- Evaluate the head of the expression on its arguments. Return Left if 
 -- no evaluation was done
evalTopLevel :: Expr -> Either Expr Expr
evalTopLevel e@(viewl.combSeq-> c :< cs)
    | argsReq > S.length cs = Left e
    | otherwise             = Right (cEvaled `appendTerms` csRest)
     where argsReq     = takesArgs c
           (as,csRest) = S.splitAt argsReq cs           
           cEvaled     = applyArgs c (F.toList as) 



    -- TRACING EVALUATION:


 -- Same as evalToNormal, but shows the evaluation steps:
traceToNormal :: Expr -> [Expr]
traceToNormal e = es ++ either traceArgs traceToNormal e' 
    where traceArgs = traceArgsWith traceToNormal
          es = raiseHeadTrace e
          e' = evalTopLevel $ last es                          



raiseHeadTrace :: Expr -> [Expr]
raiseHeadTrace e@(viewl.combSeq-> c :< cs)
    | isTerm c  = [e]
    | otherwise = let e' = Expr $ combSeq (subExpr c) >< cs
                   in e : raiseHeadTrace e'


traceArgsWith :: (Expr -> [Expr]) -> Expr -> [Expr]
traceArgsWith f = tail . traceTail . F.toList . combSeq
    where traceTail (x:xs)  = map (Expr . (x<|)) (traceArgs' xs)
          traceArgs' :: [Term] -> [Seq Term]
          traceArgs' []     = [empty]
          traceArgs' (c:cs) = [ cT <| csT  | cT <- fOnSubs c, csT <- traceArgs' cs ]
          fOnSubs t | isTerm t  = [t]
                    | otherwise = map liftSingletons $ f $ subExpr t  
          liftSingletons e@(viewl.combSeq-> c:<cs)
                    | S.null cs = c
                    | otherwise = SubExpr e
