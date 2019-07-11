{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Optional where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import qualified Prelude as P

-- | The `Optional` data type contains 0 or 1 value.
--
-- It might be thought of as a list, with a maximum length of one.
-- Like a nullable type
data Optional a =
  Full a
  | Empty
  deriving (Eq, Show)

-- | Map the given function on the possible value.
--
-- >>> mapOptional (+1) Empty
-- Empty
--
-- >>> mapOptional (+1) (Full 8)
-- Full 9
mapOptional ::
  (a -> b)
  -> Optional a
  -> Optional b

-- 1
mapOptional _ _ = Empty
mapOptional _ Empty =
  Empty
-- mapOptional f (Full a) = Full (f a)
-- 2 
mapOptional fn (Full a) = 
  Full (fn a)

-- mapOptional f (Full v) = Full (f v)
-- | Bind the given function on the possible value.
--
-- >>> bindOptional Full Empty
-- Empty
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8)
-- Full 7
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 9)
-- Full 10
bindOptional ::
  (a -> Optional b)
  -> Optional a
  -> Optional b

-- 1 if no as return no bs
bingOptional _ Empty = Empty
-- if a. put it in there. Follow type tools.
bindOptional f (Full x) =
  f x
-- tyoecheck solutions


-- eg
--if x null return null else use(x).  bindOptional use

  -- 10:15 am notes and playback 
data Three a = Three a a a
mapList :: (a-> b) -> List a -> List b
mapList _ Nil = Nil
mapList f (h:.t) = f h :. mapList f t

mapOptional :: (a -> b) -> Optinoal a -> Optional b
mapOptional _ Empty = Empty
mapOptional f (Full v) = Full (f v)

mapThree :: (a -> b) -> Three a -> Three b
mapThree f (Three a1 a2 a3) = Three (f a1) (f a2) (f a3)

flopList :: List (a -> b) -> a -> List b


                  -- list of functions     v
          -- and mapped across it   v
    -- to get access to fn
    -- turn each of those functions to a b
flopList list a = mapList (\k -> k a) list
-- flopList list_of_functions a = mapList (\k -> k a) list_of_functions

-- optional. zero or one functions
flopOptional :: Optional (a -> b) -> a -> Optional b
flopOptional o_of_functions a = mapOptional (\k -> k a) o_of_functions

flopThree :: Three (a -> b) -> a -> Three b
flopThree funcs_3 a = mapThree (\k -> k a) funcs3

-- software engineering question
-- problem?
-- Repetitive
-- wrote three times
-- called in different context


-- want to write once, allowing them to provide how they want to map
-- abstraction generic.

-- there is only one implementation that has this type
-- copy into google. haskell lazy
-- glue the two functinos together.
-- the dot
-- . function composition is a type of map.
mapTArrow :: (a -> b) -> (t -> a) -> (t -> b)
mapTArrow = (.)
flopTArrow :: (t -> (a -> b)) -> a -> (t ->b)
flopTArrow ta_funcs a = (.) (\k -> k a) ta_funcs

crazy :: (a -> b) -> Int a -> Int b -- wtf?
-- what is a List
-- constructor
-- values type system
-- kind system. Integer kind type
-- List is type 
-- :kind List
-- List :: * -> *
-- Integer :: *
-- :kind List Integer
-- arrow is infix position ? ->
-- can use prefix positino lol ->
-- List Integer :: *

-- :kind (->) :: * -> * -> *
-- KIND SYSTEM
-- >> :kind forall t. (->) t
-- forall t. (->) t :: * ->


-- >>  :kind forall a. (->) a Int
--     forall a. (->) a Int :: *
--

data NestedList a = NestedList (List (List a))
-- :kind NestedList :: * -> *

-- eg Java
abstract class AllThingsThatMap<F> {
  <A, B> F<B> mapTheThing(Func<A, B>, F<A>);
}

-- vs
class AllThingsThatMap f where
  (<$>) :: (a -> b) -> f a -> f b

instance AllThingsThatMap List where
  mapTheThings _ Nil = Nil
  mapTheThings f (h:.t) = f h :. mapTheThings f t
-- ^ can now map on Lists

flop :: f (a -> b) -> a -> f b
flop effs a = mapTheThings (\k -> k a) effs
-- ^ this can now map on all things that map
-- need constrant on f
-- as long as f has defined how to do map. instance.
-- v
flop :: AllThingsThatMap f => (a -> b) -> a -> f b
flop effs a = mapTheThings (\k -> k a) effs



-- | Return the possible value if it exists; otherwise, the second argument.
--
-- >>> Full 8 ?? 99
-- 8
--
-- >>> Empty ?? 99
-- 99
(??) ::
  Optional a
  -> a
  -> a
(??) a = a
-- Empty ?? a = a


-- | Try the first optional for a value. If it has a value, use it; otherwise,
-- use the second value.
--
-- >>> Full 8 <+> Empty
-- Full 8
--
-- >>> Full 8 <+> Full 9
-- Full 8
--
-- >>> Empty <+> Full 9
-- Full 9
--
-- >>> Empty <+> Empty
-- Empty
(<+>) ::
  Optional a
  -> Optional a
  -> Optional a
(<+>) =
  error "todo: Course.Optional#(<+>)"

-- | Replaces the Full and Empty constructors in an optional.
--
-- >>> optional (+1) 0 (Full 8)
-- 9
--
-- >>> optional (+1) 0 Empty
-- 0
optional ::
  (a -> b)
  -> b
  -> Optional a
  -> b
optional =
  error "todo: Course.Optional#optional"

applyOptional :: Optional (a -> b) -> Optional a -> Optional b
applyOptional f a = bindOptional (\f' -> mapOptional f' a) f

twiceOptional :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f = applyOptional . mapOptional f

contains :: Eq a => a -> Optional a -> Bool
contains _ Empty = False
contains a (Full z) = a == z

instance P.Functor Optional where
  fmap =
    M.liftM

instance A.Applicative Optional where
  (<*>) =
    M.ap
  pure =
    Full

instance P.Monad Optional where
  (>>=) =
    flip bindOptional
  return =
    Full
