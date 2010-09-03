module Main
    where


import CombinatorCalculus


main = do
     -- a random combinator expression: 
    let e = cI · (cI · S) · (K · S) · K · cI · S · cI  

    printSteps $ traceToNormal e

    putStrLn "------------------------------------"
     -- the number 2 as a Church Numeral in the combinator calculus. This time
     -- we use a new type for I, instead of defining `I` in terms of `S` & `K`:
    let two = S·(S·(K·S)·K)·I
    
     -- ...and "rendered" through application on two combinators `f` and `x`
     -- and evaluation:
    printSteps $ traceToNormal $ two · f · x

    putStrLn "------------------------------------"
     -- Here is a more verbose derivation of 2 in the combinator calculus:
    let two' = S·(S·(K·S)·(S·(K·K)·I)) · (S·(S·(K·S)·(S·(K·K)·I))·(K·I))
    printSteps $ traceToNormal $ two' · f · x
 


-- a little helper for printing evaluation steps:
printSteps :: [Expr] -> IO ()
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

 -- ...or we can define an `I` type as we did with `S` and `K` above:
data I = I deriving Show
instance Combinator I where
    takesArgs _ = 1
    applyArgs _ [a] = expr a 



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
