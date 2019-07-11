{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Functor where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import qualified Prelude as P(fmap)

-- | All instances of the `Functor` type-class must satisfy two laws. These laws
-- are not checked by the compiler. These laws are given as:
--
-- * The law of identity
--   `∀x. (id <$> x) ≅ x`
--
-- * The law of composition
-- upside down A char
-- for all
-- for all f g and f x,

--   `∀f g x.(f . g <$> x) ≅ (f <$> (g <$> x))`
class Functor f where
  -- Pronounced, eff-map.
  (<$>) ::
    (a -> b)
    -> f a
    -> f b

infixl 4 <$>

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Course.Core
-- >>> import qualified Prelude as P(return, (>>))

-- | Maps a function on the ExactlyOne functor.
--
-- >>> (+1) <$> ExactlyOne 2
-- ExactlyOne 3
instance Functor ExactlyOne where
  (<$>) ::
    (a -> b)
    -> ExactlyOne a
    -> ExactlyOne b
  
  f <$> ExactlyOne a =
    ExactlyOne (f a)
 
-- -- cheat code `map` or pattern match
-- f <$> ExactlyOne a =
--   ExactlyOne (f a)

-- | Maps a function on the List functor.
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Functor List where
  (<$>) ::
    (a -> b)
    -> List a
    -> List b
  (<$>) =
    error "todo: Course.Functor (<$>)#instance List"

-- | Maps a function on the Optional functor.
--
-- >>> (+1) <$> Empty
-- Empty
--
-- >>> (+1) <$> Full 2
-- Full 3
instance Functor Optional where
  (<$>) ::
    (a -> b)
    -> Optional a
    -> Optional b
  (<$>) =
    mapOptional

-- paper based aids and move around.
-- rewrite signature to make easier to read
-- change from prefix to infix operator.
-- instance Functor ((->) t) where
--   (<$>) ::
--     (a -> b)
--     -> (t -> a)
--     -> (t -> b)
--   (<$>) =
--     error "todo: Course.Functor (<$>)#((->) t)"

-- | Maps a function on the reader ((->) t) functor.
--
-- >>> ((+1) <$> (*2)) 8
-- 17
instance Functor ((->) t) where
  (<$>) ::
    (a -> b)
    -> ((->) t a)
    -> ((->) t b)
  -- (<$>) = \x -> f ()
  --  error "todo: Course.Functor (<$>)#((->) t)"
  -- idk what to do
  -- use underscore for idk :P
  -- hole
  -- type lambda \
  -- type \admaslmkd -> _
  -- reload
  -- thingthing then -> underscore
  -- then needs a  bbbbbeee
  -- src/Course/Functor.hs:117:35: error:
  --   • Found hole: _ :: b
  --     Where: ‘b’ is a rigid type variable bound by
  --              the type signature for:
  --                (<$>) :: forall a b. (a -> b) -> (t -> a) -> t -> b
  --              at src/Course/Functor.hs:(103,5)-(105,17)
  --   • In the expression: _
  --     In the expression: \ dshdb -> _
  --     In the expression: \ fxgn -> \ dshdb -> _
  --   • Relevant bindings include
  --       dshdb :: t (bound at src/Course/Functor.hs:117:26)
  --       fxgn :: t -> a (bound at src/Course/Functor.hs:117:17)
  --       rxtjrn :: a -> b (bound at src/Course/Functor.hs:117:6)
  --       (<$>) :: (a -> b) -> (t -> a) -> t -> b
  --         (bound at src/Course/Functor.hs:116:3)

  -- followed the types to the answer
  (<$>) =
    \atob -> \ddd -> \dshdb -> atob (ddd dshdb)
  -- put it into hoogle...
  -- f <$> g =
  --   \x -> f (g x)
-- use lambda for function
-- fn  a -> b   -> t a     return b
-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ (1 :. 2 :. 3 :. Nil)
-- [7,7,7]
--
-- prop> \x a b c -> x <$ (a :. b :. c :. Nil) == (x :. x :. x :. Nil)
--
-- prop> \x q -> x <$ Full q == Full x
(<$) ::
  Functor f =>
  a
  -> f b
  -> f a
(<$) = 
  -- _ -- need a function
  -- \thisIsAnA -> _ -- need a function
  -- \thisA -> \thisIsf_b -> _ -- ned a func
  -- \a -> \f_b -> _ <$> f_b -- ? need a functino from b to a
  -- \a -> \f_b -> (\b -> _) <$> f_b -- now need an a
  \a -> \f_b -> (\b -> a) <$> f_b -- now have an a
  
  -- found hole
  -- fmap if can turn x into y can turn f(x) into f(y)

  -- (<$>) . const

-- f of anything - can call fmap

-- fmap on f
-- const :: a -> b -> a
-- (<$>) . const
-- turn b into a
-- ignore b . return a.
-- map across a list and don't return a function e.g.
  -- fmap across this f b
  -- need to give it a function 
  -- ignore the b
  -- return the a
  -- can call fmap on the f. the f that fmap. instance Functor where fmap

  -- error "todo: Course.Functor#(<$)"

-- | Anonymous map producing unit value.
--
-- >>> void (1 :. 2 :. 3 :. Nil)
-- [(),(),()]
--
-- >>> void (Full 7)
-- Full ()
--
-- >>> void Empty
-- Empty
--
-- >>> void (+10) 5
-- ()
void ::
  Functor f =>
  f a
  -> f ()
void =
  _
-- what does fmap do :D
-- turn a into unit ()
-- () unit
-- Found hole: _ :: f a -> f ()

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

-- | Maps a function on an IO program.
--
-- >>> reverse <$> (putStr "hi" P.>> P.return ("abc" :: List Char))
-- hi"cba"
instance Functor IO where
  (<$>) =
    P.fmap
