{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Origin.Het.Variant where

import           Data.Kind                   (Type, Constraint)

import qualified Data.Barbie                 as B

import           Data.Origin.Het.All
import           Data.Origin.Het.Nat
import           Data.Origin.Options.Types


data VariantF (xs :: [(Type -> Type) -> Type]) (f :: Type -> Type) where
  HereF  :: x f           -> VariantF (x ': xs) f
  ThereF :: VariantF xs f -> VariantF (y ': xs) f

instance ( B.FunctorB x
         , B.FunctorB (VariantF xs)
         ) => B.FunctorB (VariantF (x ': xs)) where
  bmap nat (HereF x)   = HereF $ B.bmap nat x
  bmap nat (ThereF xs) = ThereF $ B.bmap nat xs

instance B.FunctorB (VariantF '[]) where
  bmap _ _ = error "Impossible: empty variant"

instance ( B.TraversableB x
         , B.TraversableB (VariantF xs)
         ) => B.TraversableB (VariantF (x ': xs)) where
  btraverse nat (HereF x)   = HereF <$> B.btraverse nat x
  btraverse nat (ThereF xs) = ThereF <$> B.btraverse nat xs

instance B.TraversableB (VariantF '[]) where
  btraverse _ _ = error "Impossible: empty variant"

deriving instance AllF Show xs OptValue => Show (VariantF xs OptValue)

instance AllF Semigroup xs OptValue => Semigroup (VariantF xs OptValue) where
  HereF  x <> HereF  y = HereF  (x <> y)
  ThereF x <> ThereF y = ThereF (x <> y)

type family FoldSignatureF (xs :: [(Type -> Type) -> Type]) r f where
  FoldSignatureF (x ': xs) r f = (x f -> r) -> FoldSignatureF xs r f
  FoldSignatureF '[] r f = r

class BuildFoldF xs result f where
  foldF :: VariantF xs f -> FoldSignatureF xs result f

instance BuildFoldF '[x] result f where
  foldF (HereF  x) f = f x
  foldF (ThereF _) _ = error "Impossible: empty variant"

instance ( tail ~ (x' ': xs)
         , BuildFoldF tail result f
         , IgnoreF tail result f
         ) => BuildFoldF (x ': x' ': xs) result f where
  foldF (ThereF x) _ = foldF @_ @result x
  foldF (HereF  x) f = ignoreF @tail (f x)

class IgnoreF (args :: [(Type -> Type) -> Type]) result f where
  ignoreF :: result -> FoldSignatureF args result f

instance IgnoreF '[] result f where
  ignoreF result = result

instance IgnoreF xs result f => IgnoreF (x ': xs) result f where
  ignoreF result _ = ignoreF @xs @_ @f result

class InjectPosF
    (n :: Nat)
    (x :: (Type -> Type) -> Type)
    (xs :: [(Type -> Type) -> Type]) where
  injectPosF :: SNat n -> (x f -> VariantF xs f)

instance InjectPosF Z x (x ': xs) where
  injectPosF SZ = HereF

instance InjectPosF n x xs => InjectPosF (S n) x (y ': xs) where
  injectPosF (SS n) = ThereF . injectPosF n
