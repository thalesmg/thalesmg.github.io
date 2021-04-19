---
title: Optimizing meal allowance card usage using SBV
tags: haskell, smt solvers, sbv
---

I was faced with the following problem: find a grocery shopping list
that optimally consumes all remaining credit in a meal allowance
card. The company I work for switched card providers and stopped
crediting the old card, and I wanted to fully consume its remaining
balance to the last cent.

So, I had the idea to use the [Z3
solver](https://github.com/Z3Prover/z3) to extract a subset of my
desired shopping list such that the total amount, shipping included,
would approach the remaining card balance as close as possible. I
chose Haskell as the language, which has the excellent
[SBV](https://hackage.haskell.org/package/sbv) package that provides
an API for SMT solvers such as Z3.

For this, I used a few language pragmas:

```haskell
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
```

... and a few imports:

```haskell
import Data.Foldable (traverse_)
import Data.SBV
import Data.SBV.Control
import Data.Functor.Identity (Identity(..))
```

Following a hint from the [Murder puzzle
example](https://hackage.haskell.org/package/sbv-8.14/docs/Documentation-SBV-Examples-Puzzles-Murder.html#t:Person)
from SBV (which seems like an application of [higher-kinded
data](https://reasonablypolymorphic.com/blog/higher-kinded-data/)), I
parameterized my shopping item over the representation for the value:

```haskell
data Item f = MkItem { itemID :: String
                     , value :: f Integer
                     }

instance Show (Item Identity) where
  show MkItem{itemID, value = Identity value} =
    "(" <> itemID <> ", " <> show value <> ")"
```

By parameterizing `Item` over `f`, we can use the same structure both
in concrete (when extracting solutions) and in symbolic (when
searching for solutions) contexts. SBV symbolic values are wrapped in
the `SBV` type, while the concrete ones are simply wrapped inside
`Identity`. To transform a symbolic item into a concrete one, we must
query a (satisfiable) model and get the values from it, which is done
by `getValue`:

```haskell
getItem :: Item SBV -> Query (Item Identity)
getItem MkItem{itemID, value} =
  MkItem itemID <$> (fmap Identity . getValue $ value)
```

At first, I approached the problem trying to figure out how to express
possible subsets of a given set in SBV. I could not figure out how to
do it this way, since the resulting set could have any number of
items, and was having trouble trying to sketch the function types
using
[`SSet`](https://hackage.haskell.org/package/sbv-8.14/docs/Data-SBV.html#t:SSet)
and `Item SBV`.

Then I had a much, much simpler idea. I simply had to create one
`Bool` for each item in the list to represent whether it should be
selected. This may seem quite obvious in retrospect, but it took some
time for me to formulate the problem in an amenable way to the
solver. I hope by sharing it here it may help someone else facing a
similar problem.

Anyway, the final solution is presented below:

```haskell
split :: [Item SBV]
         -- ^ current shopping list
      -> SInteger
         -- ^ shipping cost
      -> (SInteger, SInteger)
         -- ^ (minimum, maximum) values of the sublist, to allow
         -- some flexibility in the solution
      -> IO [Item Identity]
         -- ^ the final sublist that approximates the card
         -- balance, if any
split originalList shipping (mini, maxi) = runSMT $ do
  -- we create a bool for each item in the input list
  bs <- sBools . map itemID $ originalList
  -- the symbolic total value of the sublist, yet to be determined
  let total = foldr (\(b, x) acc -> oneIf b * value x + acc)
                    shipping (zip bs originalList)
  -- the total value of the sublist should be within the
  -- acceptable bounds
  constrain $ mini .<= total .&& total .<= maxi
  query $ do
    -- is the model satisfiable?
    cs <- checkSat
    case cs of
      -- we can only probe the model if it is satisfiable
      Sat -> do
        -- we make the booleans concrete
        bs' <- mapM getValue bs
        -- ... and also the input values, even if they are
        -- literals to begin with
        xs <- mapM getItem originalList
        s <- getValue shipping
        let xs' = map snd
                . filter fst
                . zip bs'
                $ xs
            total = sum . map (runIdentity . value) $ xs'
        io $ putStr "Total (only items) = "
        io . print $ total
        io $ putStr "Total (items + shipping) = "
        io . print $ total + s
        pure xs'
      -- if the model is provably unsatisfiable or if the
      -- result is unknown (when the solver cannot prove
      -- nor disprove the assertions)
      -- we simply print a message and return an empty list
      _ -> do
        io $ putStrLn "Impossible!"
        pure []
```

I already had a shopping list I wanted to buy for the month, and then
I tried to apply the solver and see if I could optimize the allowance
usage. To my surprise, even after altering the list more than once due
to items becoming unavailable or correcting rounding errors in the
values, the solver could find solutions that _exactly_ consumed the
entire balance!

To get the solution:

```haskell
> result <- split exampleList 14_90 (200_00, 210_00)
Total (only items) = 18756
Total (items + shipping) = 20246
> traverse_ print result
(Bread, 949)
(Butter, 679)
(Milk, 798)
(Coffee, 2198)
(Decaffeinated Coffee, 2198)
(Hamburger, 2179)
(Sausage, 1055)
(Cheese, 2408)
(Bleach, 3045)
(Sponge, 499)
(Toothbrush, 1049)
(Flour, 1699)
```

Here's a sample input list with some fake items to try out. We use
`Item SBV` with integer literals so the solver can reason about those
values. Also, values are represented in cents:

<details>
<summary>Sample shopping list:</summary>
```haskell
exampleList :: [Item SBV]
exampleList = [ MkItem "Rice" 13_99
              , MkItem "Beans" 7_99
              , MkItem "Oil" 68_97
              , MkItem "Bread" 9_49
              , MkItem "Butter" 6_79
              , MkItem "Milk" 7_98
              , MkItem "Coffee" 21_98
              , MkItem "Decaffeinated Coffee" 21_98
              , MkItem "Hamburger" 21_79
              , MkItem "Hot dog" 21_79
              , MkItem "Meat" 28_59
              , MkItem "Chicken" 15_97
              , MkItem "Sausage" 10_55
              , MkItem "Cheese" 24_08
              , MkItem "Bleach" 30_45
              , MkItem "Sponge" 4_99
              , MkItem "Soap" 28_19
              , MkItem "Soda" 35_80
              , MkItem "Toothbrush" 10_49
              , MkItem "Flour" 16_99
              ]
```
</details>
