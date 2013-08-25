{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

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

-- * Units

data Duration = Days | Months | Years deriving (Show)
data Size = Bytes | KBytes | MBytes deriving (Show)

-- | Render a search expression using Google search syntax.
class SearchBuilder e where
    searchBuilder :: e -> Builder

-- | 'SearchBuilder' for 'Free'!
instance (Functor f, SearchBuilder a, SearchBuilder (f Builder)) =>
        SearchBuilder (Free f a) where
    searchBuilder = iter searchBuilder . fmap searchBuilder

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

-- | The shape of Boolean expressions.
data BooleanF e = Not e | And [e] | Or [e] deriving (Functor, Show)

instance SearchBuilder (BooleanF Builder) where
    searchBuilder b = case b of
        Not e -> "-" <> e
        And es -> ("(" <>) . (<> ")") . fold $ intersperse " " es
        Or es -> ("(" <>) . (<> ")") . fold $ intersperse " OR " es

-- | The free Boolean-shaped monad: comes with an 'IsString' instance
-- so we can write @\"simple queries\" :: 'Simple'@. No refunds.
type BooleanM = Free BooleanF

instance (IsString a) => IsString (BooleanM a) where
    fromString = return . fromString

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

