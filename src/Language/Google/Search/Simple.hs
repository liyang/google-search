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
import Data.Foldable (fold)
import Data.List (intersperse)
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

-- | Render a search expression using Google search syntax.
class SearchBuilder e where
    searchBuilder :: e -> Builder

-- | Higher-order version of 'SearchBuilder'.
class SyntaxBuilder f where
    syntaxBuilder :: f Builder -> Builder

-- | 'SearchBuilder' for 'Free'!
instance (Functor f, SearchBuilder a, SyntaxBuilder f) =>
        SearchBuilder (Free f a) where
    searchBuilder = iter syntaxBuilder . fmap searchBuilder

------------------------------------------------------------------------
-- * Primitive Terms

-- | 'Fuzzy' terms are grouped with parentheses (if necessary), while
-- 'Exact' terms are always “double-quoted”. The 'IsString' instance
-- defaults to 'Fuzzy', so a @\"literal string\" ∷ 'Term'@ may be used.
data Term = Fuzzy Text | Exact Text deriving (Show)
instance IsString Term where fromString = Fuzzy . T.pack

instance SearchBuilder Term where
    searchBuilder term = case term of
        Fuzzy s -> ($ B.fromText s) $
            if T.any isSpace s then ("(" <>) . (<> ")") else id
        Exact s -> ($ B.fromText s) $
            (B.singleton '"' <>) . (<> B.singleton '"')

------------------------------------------------------------------------
-- * Boolean expressions

-- | The shape of Boolean expressions.
data BooleanF e = Not e | And [e] | Or [e] deriving (Functor, Show)

instance SyntaxBuilder BooleanF where
    syntaxBuilder bool = case bool of
        Not e -> "-" <> e
        And es -> ("(" <>) . (<> ")") . fold $ intersperse " " es
        Or es -> ("(" <>) . (<> ")") . fold $ intersperse " OR " es

-- | The free Boolean-shaped monad. No refunds.
type BooleanM = Free BooleanF

-- | Simple Boolean combinations of 'Term's.
type Simple = BooleanM Term

------------------------------------------------------------------------

-- | Conjunction on a list of 'BooleanM' expressions.
fAnd :: [BooleanM a] -> BooleanM a
fAnd = Free . And

-- | Disjunction on a list of 'BooleanM' expressions.
fOr :: [BooleanM a] -> BooleanM a
fOr = Free . Or

-- | Negation of a 'BooleanM' expression.
fNot :: BooleanM a -> BooleanM a
fNot = Free . Not

