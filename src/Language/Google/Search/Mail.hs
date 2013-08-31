{-# LANGUAGE OverloadedStrings #-}

-- | <https://support.google.com/mail/answer/7190>

module Language.Google.Search.Mail where

import Prelude
import Data.Monoid
import Data.String
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import Data.Time.Calendar (Day)
import Data.Time.Format (formatTime)
import Numeric.Natural
import System.Locale (defaultTimeLocale)

import Language.Google.Search.Simple

-- | Features for the @has:@ operator.
data Feature
    = Attachment
    | NoUserLabels
    | UserLabels
    -- stars
    | BlueInfo
    | BlueStar
    | Circle
    | GreenCheck
    | GreenStar
    | OrangeGuillemet
    | OrangeStar
    | PurpleQuestion
    | PurpleStar
    | RedBang
    | RedStar
    | YellowBang
    | YellowStar
    deriving (Show)

-- | Locations for the @in:@ operator.
data Location
    = Anywhere
    | Inbox
    | Trash
    | Spam
    deriving (Show)

-- | Statuses for the @is:@ operator.
data Status
    = Important
    | Starred
    | Unread
    | Read
    | Chat
    deriving (Show)

-- | Categories for the @category:@ operator.
data Category
    = Forums
    | Personal
    | Promotions
    | Social
    | Updates
    deriving (Show)

-- | GMail search operators.
data MailOp
    = Plain Simple
    | From Simple
    | To Simple
    | Subject Simple
    | Label Text
    | Has Feature
    | List Simple
    | Filename Simple
    | In Location
    | Is Status
    | Cc Simple
    | Bcc Simple
    | After Day
    | Before Day
    | Older Natural Duration
    | Newer Natural Duration
    | DeliveredTo Simple
    | FromCircle Simple
    | Category Category
    | Larger Natural Size
    | Smaller Natural Size
    | RFC822MsgId Text
    deriving (Show)

instance IsString MailOp where
    fromString = Plain . fromString

-- | Boolean combinations of GMail operators or 'Plain' 'Simple' terms.
type Mail = BooleanM MailOp

instance SearchBuilder MailOp where
    searchBuilder operator = case operator of
        Plain s         -> searchBuilder    s
        From s          -> "from:"          <>? s
        To s            -> "to:"            <>? s
        Subject s       -> "subject:"       <>? s
        Label t         -> "label:"         <>! B.fromText t
        Has ft          -> "has:"           <>! case ft of
            Attachment      -> "attachment"
            BlueInfo        -> "blue-info"
            BlueStar        -> "blue-star"
            Circle          -> "circle"
            GreenCheck      -> "green-check"
            GreenStar       -> "green-star"
            NoUserLabels    -> "nouserlabels"
            OrangeGuillemet -> "orange-guillemet"
            OrangeStar      -> "orange-star"
            PurpleQuestion  -> "purple-question"
            PurpleStar      -> "purple-star"
            RedBang         -> "red-bang"
            RedStar         -> "red-star"
            UserLabels      -> "userlabels"
            YellowBang      -> "yellow-bang"
            YellowStar      -> "yellow-star"
        List s          -> "list:"          <>? s
        Filename s      -> "filename:"      <>? s
        In l            -> "in:"            <>! case l of
            Anywhere        -> "anywhere"
            Inbox           -> "inbox"
            Trash           -> "trash"
            Spam            -> "spam"
        Is s            -> "is:"            <>! case s of
            Important       -> "important"
            Starred         -> "starred"
            Unread          -> "unread"
            Read            -> "read"
            Chat            -> "chat"
        Cc s            -> "cc:"            <>? s
        Bcc s           -> "bcc:"           <>? s
        After d         -> "after:"         <>! date d
        Before d        -> "before:"        <>! date d
        Older n d       -> "older_than:"    <>! duration n d
        Newer n d       -> "newer_than:"    <>! duration n d
        DeliveredTo s   -> "deliveredto:"   <>? s
        FromCircle s    -> "circle:"        <>? s
        Category c      -> "category:"      <>! case c of
            Forums          -> "forums"
            Personal        -> "personal"
            Promotions      -> "promotions"
            Social          -> "social"
            Updates         -> "updates"
        Larger n u      -> "larger:"        <>! size n u
        Smaller n u     -> "smaller:"       <>! size n u
        RFC822MsgId t   -> "rfc822msgid:"   <>! B.fromText t

      where
        infix 6 <>?, <>!

        (<>?) :: (SearchBuilder e) => Builder -> e -> PrecBuilder
        name <>? arg = parentheses 10 $ \ p -> name <> p (searchBuilder arg)

        (<>!) :: Builder -> Builder -> PrecBuilder
        name <>! arg = PrecBuilder 10 (name <> arg)

        date :: Day -> Builder
        date = B.fromString . formatTime defaultTimeLocale "%Y/%m/%d"

        duration :: Natural -> Duration -> Builder
        duration n d = B.decimal n <> case d of
            Days -> "d"
            Months -> "m"
            Years -> "y"

        size :: Natural -> Size -> Builder
        size n u = B.decimal n <> case u of
            Bytes -> mempty
            KBytes -> "k"
            MBytes -> "m"

