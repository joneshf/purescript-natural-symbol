module Type.Natural.Symbol
  ( class IsNatural
  , class ValidNatural
  , reflectNatural
  , reflectValidNatural
  ) where

import Prelude

import Data.FoldableWithIndex
  ( foldrWithIndex
  )
import Data.Int
  ( pow
  )
import Data.List
  ( List
  , reverse
  , (:)
  )
import Prim.Symbol
  ( class Cons
  )
import Prim.TypeError
  ( class Fail
  , Text
  )
import Type.Eval
  ( type (<>)
  )

class IsNatural (natural :: Symbol) where
  reflectNatural ::
    forall proxy.
    proxy natural ->
    Int

instance isNotNatural ::
  ( Fail (Text "The empty Symbol is not a natural")
  ) =>
  IsNatural "" where
    reflectNatural x = reflectNatural x

else instance isNatural ::
  ( Cons head tail digits
  , ValidNatural head tail digits
  ) =>
  IsNatural digits where
    reflectNatural _ = foldrWithIndex go 0 (reverse digits)
      where
      digits = reflectValidNatural (NProxy :: NProxy head) (NProxy :: NProxy tail) (NProxy :: NProxy digits)
      go i x acc = x * pow 10 i + acc

class ValidNatural (head :: Symbol) (tail :: Symbol) (original :: Symbol) where
  reflectValidNatural ::
    forall proxy.
    proxy head ->
    proxy tail ->
    proxy original ->
    List Int

instance validNatural0 ::
  ValidNatural "0" "" a where
    reflectValidNatural _ _ _ = pure 0

else instance validNatural1 ::
  ValidNatural "1" "" a where
    reflectValidNatural _ _ _ = pure 1

else instance validNatural2 ::
  ValidNatural "2" "" a where
    reflectValidNatural _ _ _ = pure 2

else instance validNatural3 ::
  ValidNatural "3" "" a where
    reflectValidNatural _ _ _ = pure 3

else instance validNatural4 ::
  ValidNatural "4" "" a where
    reflectValidNatural _ _ _ = pure 4

else instance validNatural5 ::
  ValidNatural "5" "" a where
    reflectValidNatural _ _ _ = pure 5

else instance validNatural6 ::
  ValidNatural "6" "" a where
    reflectValidNatural _ _ _ = pure 6

else instance validNatural7 ::
  ValidNatural "7" "" a where
    reflectValidNatural _ _ _ = pure 7

else instance validNatural8 ::
  ValidNatural "8" "" a where
    reflectValidNatural _ _ _ = pure 8

else instance validNatural9 ::
  ValidNatural "9" "" a where
    reflectValidNatural _ _ _ = pure 9

else instance validNatural0Rest ::
  ( Cons head tail rest
  , ValidNatural head tail original
  ) =>
  ValidNatural "0" rest original where
    reflectValidNatural _ _ _ = 0 : reflectValidNatural (NProxy :: NProxy head) (NProxy :: NProxy tail) (NProxy :: NProxy original)

else instance validNatural1Rest ::
  ( Cons head tail rest
  , ValidNatural head tail original
  ) =>
  ValidNatural "1" rest original where
    reflectValidNatural _ _ _ = 1 : reflectValidNatural (NProxy :: NProxy head) (NProxy :: NProxy tail) (NProxy :: NProxy original)

else instance validNatural2Rest ::
  ( Cons head tail rest
  , ValidNatural head tail original
  ) =>
  ValidNatural "2" rest original where
    reflectValidNatural _ _ _ = 2 : reflectValidNatural (NProxy :: NProxy head) (NProxy :: NProxy tail) (NProxy :: NProxy original)

else instance validNatural3Rest ::
  ( Cons head tail rest
  , ValidNatural head tail original
  ) =>
  ValidNatural "3" rest original where
    reflectValidNatural _ _ _ = 3 : reflectValidNatural (NProxy :: NProxy head) (NProxy :: NProxy tail) (NProxy :: NProxy original)

else instance validNatural4Rest ::
  ( Cons head tail rest
  , ValidNatural head tail original
  ) =>
  ValidNatural "4" rest original where
    reflectValidNatural _ _ _ = 4 : reflectValidNatural (NProxy :: NProxy head) (NProxy :: NProxy tail) (NProxy :: NProxy original)

else instance validNatural5Rest ::
  ( Cons head tail rest
  , ValidNatural head tail original
  ) =>
  ValidNatural "5" rest original where
    reflectValidNatural _ _ _ = 5 : reflectValidNatural (NProxy :: NProxy head) (NProxy :: NProxy tail) (NProxy :: NProxy original)

else instance validNatural6Rest ::
  ( Cons head tail rest
  , ValidNatural head tail original
  ) =>
  ValidNatural "6" rest original where
    reflectValidNatural _ _ _ = 6 : reflectValidNatural (NProxy :: NProxy head) (NProxy :: NProxy tail) (NProxy :: NProxy original)

else instance validNatural7Rest ::
  ( Cons head tail rest
  , ValidNatural head tail original
  ) =>
  ValidNatural "7" rest original where
    reflectValidNatural _ _ _ = 7 : reflectValidNatural (NProxy :: NProxy head) (NProxy :: NProxy tail) (NProxy :: NProxy original)

else instance validNatural8Rest ::
  ( Cons head tail rest
  , ValidNatural head tail original
  ) =>
  ValidNatural "8" rest original where
    reflectValidNatural _ _ _ = 8 : reflectValidNatural (NProxy :: NProxy head) (NProxy :: NProxy tail) (NProxy :: NProxy original)

else instance validNatural9Rest ::
  ( Cons head tail rest
  , ValidNatural head tail original
  ) =>
  ValidNatural "9" rest original where
    reflectValidNatural _ _ _ = 9 : reflectValidNatural (NProxy :: NProxy head) (NProxy :: NProxy tail) (NProxy :: NProxy original)

else instance isNotValidNatural ::
  ( Fail (Text "`" <> Text notNatural <> Text "` is not a valid natural")
  ) =>
  ValidNatural a b notNatural where
    reflectValidNatural x = reflectValidNatural x

data NProxy (natural :: Symbol)
  = NProxy

zero ::
  Int
zero = reflectNatural (NProxy :: NProxy "0")

one ::
  Int
one = reflectNatural (NProxy :: NProxy "1")

tweleve ::
  Int
tweleve = reflectNatural (NProxy :: NProxy "12")
