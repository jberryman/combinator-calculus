module Main
    where


import CombinatorCalculus


main = do
     -- a random combinator expression: S(KS)KISI:
    let e = cI · (cI · S) · (K · S) · K · cI · S · cI  
    printSteps $ evalToNormal e
   
    putStrLn "------------------------------------"
 
     -- the number 2 as a Church Numeral in the combinator calculus:
    let two = S·(S·(K·S)·K)·cI
    
     -- ...and "rendered" through application on two combinators `f` and `x`
     -- and evaluation:
    printSteps $ evalToNormal $ two · f · x



-- a little helper for printing evaluation steps:
printSteps = mapM_ putStrLn . zipWith prettify [1..] 
    where prettify n e = show n ++". \t"++ show e




    -----------------------
    -- DEFINED COMBINATORS:
    -----------------------


data S = S deriving Show
instance Combinator S where
    takesArgs _ = 3
    applyArgs _ [a,b,c] = a · c · (b · c)


data K = K deriving Show
instance Combinator K where
    takesArgs _ = 2
    applyArgs _ [a,_] = expr a 


 -- the "Identity" combinator `I`, defined in terms of `S` and `K`. Evaluating
 -- this combinator may substitute (S K K) for `I`, or that step may be hidden
 -- depending on how the evaluation function was defined:
cI = (S · K · K)  `named` "I"


 -- some "quasi-combinators" that let us play with Church Numerals encoded in 
 -- the Combinator calculus. They will look like: f (f (f (f x)))
data F = F
instance Show F where
    show F = "f"

instance Combinator F where
    takesArgs F = 2
    applyArgs F [_,b] = expr b


f = F
x = expr cI `named` "x"    -- The combinator doesn't matter. We chose `I`
