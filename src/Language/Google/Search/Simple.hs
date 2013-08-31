{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | An orphan lives in this module,
--
-- @
-- instance ('Functor' f, 'IsString' a) => 'IsString' ('Free' f a)
-- @
--
-- so that we can write @\"simple queries\" :: 'Simple'@.

module Language.Google.Search.Simple where

import Prelude
import Control.Monad.Free
import Data.Char (isSpace)
import Data.Monoid
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B

instance (Functor f, IsString a) => IsString (Free f a) where
    fromString = return . fromString

------------------------------------------------------------------------
-- * Units

data Duration = Days | Months | Years deriving (Show)
data Size = Bytes | KBytes | MBytes deriving (Show)

------------------------------------------------------------------------
-- * Search expression construction

-- | 'Builder' with precedence, though ambiguous associativity.
-- (But that's okay because Google doesn't mind which way you lean.)
--
-- Note that at Google @OR@ binds tighter than conjunction, which is flipped
-- in contrast to everywhere else. We take the analogous Haskell fixities
-- when building search expressions:
--
-- * 11: atomic tokens or parenthesised expressions
--
-- * 10: complementation, search operators (cf. Haskell prefix application)
--
-- * 3: disjunction (@OR@ or @|@)
--
-- * 2: conjunction (by juxtaposition)
data PrecBuilder = PrecBuilder Int Builder deriving (Show)

-- | Give me the precedence of the piece of syntax you're building, and I'll
-- give you a function that parenthesise any sub-expressions when necessary.
parentheses :: Int ->
    ((PrecBuilder -> Builder) -> Builder) -> PrecBuilder
parentheses outer f = PrecBuilder outer . f $ \ (PrecBuilder inner e) ->
    if outer > inner then "(" <> e <> ")" else e

-- | Render a search expression using Google search syntax.
class SearchBuilder e where
    searchBuilder :: e -> PrecBuilder

-- | Higher-order version of 'SearchBuilder'.
class SyntaxBuilder f where
    syntaxBuilder :: f PrecBuilder -> PrecBuilder

-- | 'SearchBuilder' for 'Free'!
instance (Functor f, SearchBuilder a, SyntaxBuilder f) =>
        SearchBuilder (Free f a) where
    searchBuilder = iter syntaxBuilder . fmap searchBuilder

------------------------------------------------------------------------
-- * Primitive Terms

-- | 'Fuzzy' terms are grouped with parentheses (if necessary), while
-- 'Exact' terms are always “double-quoted”. The 'IsString' instance
-- defaults to 'Fuzzy', so just writing @\"literal string\" ∷ 'Term' 'Text'@
-- is acceptable.
data Term t = Fuzzy t | Exact t deriving (Functor, Show)
instance (IsString t) => IsString (Term t) where fromString = Fuzzy . fromString

instance SearchBuilder Text where
    searchBuilder t = PrecBuilder prec (B.fromText t) where
        prec = if T.any isSpace t then 2 else 11

instance SyntaxBuilder Term where
    syntaxBuilder term = case term of
        Fuzzy e -> e
        Exact (PrecBuilder _ e) -> PrecBuilder 11 $ "\"" <> e <> "\""

instance (SearchBuilder a) => SearchBuilder (Term a) where
    searchBuilder = syntaxBuilder . fmap searchBuilder

------------------------------------------------------------------------
-- * Boolean expressions

-- | The shape of Boolean expressions.
infixr 3 `AndB`
infixr 2 `OrB`
data BooleanF e = NotB e | e `AndB` e | e `OrB` e deriving (Functor, Show)

instance SyntaxBuilder BooleanF where
    syntaxBuilder bool = case bool of
        NotB e -> parentheses 10 $ \ p -> "-" <> p e
        a `AndB` b -> parentheses 2 $ \ p -> p a <> " "    <> p b
        a `OrB`  b -> parentheses 3 $ \ p -> p a <> " OR " <> p b

-- | The free Boolean-shaped monad. No refunds.
type BooleanM = Free BooleanF

-- | Simple Boolean combinations of 'Term's.
type Simple = BooleanM (Term Text)

