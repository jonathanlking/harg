{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Options.Harg.Construct.Internal
  ( NotInAttrs,
    CommaSep,
    CommaSep',
    QuoteSym,
    DuplicateAttrErr,
    DuplicateAttrMultipleErr,
    IncompatibleAttrsErr,
  )
where

import Data.Kind (Constraint)
import GHC.TypeLits (AppendSymbol, ErrorMessage (..), Symbol, TypeError)

-- | Wrap a symbol in quotes, for pretty printing in type errors.
type QuoteSym (s :: Symbol) =
  'Text "`" :<>: 'Text s :<>: 'Text "`"

-- | Check if `x` is not an element of the type-level list `xs`. If it is
-- print the appropriate error message using `l` and `r` for clarity.
type family
  NotInAttrs
    (x :: k)
    (xs :: [k])
    (err :: ErrorMessage) ::
    Constraint
  where
  NotInAttrs _ '[] _ =
    ()
  NotInAttrs x (x ': _) err =
    TypeError err
  NotInAttrs x (y ': xs) err =
    NotInAttrs x xs err

type family CommaSep (xs :: [Symbol]) :: Symbol where
  CommaSep '[] = ""
  CommaSep '[x] = " or " `AppendSymbol` x
  CommaSep (x ': xs) = " or one of " `AppendSymbol` CommaSep' x xs

type family CommaSep' (s :: Symbol) (xs :: [Symbol]) :: Symbol where
  CommaSep' s '[] = s
  CommaSep' s (x ': xs) = CommaSep' (s `AppendSymbol` ", " `AppendSymbol` x) xs

type DuplicateAttrErr attr =
  QuoteSym attr
    :<>: 'Text " is already specified."

type DuplicateAttrMultipleErr attr rest =
  QuoteSym attr
    :<>: 'Text (CommaSep rest)
    :<>: 'Text " has already been specified."

type IncompatibleAttrsErr l r =
  QuoteSym l
    :<>: 'Text " and "
    :<>: QuoteSym r
    :<>: 'Text " cannot be mixed in an option definition."
