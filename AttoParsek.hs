{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module AttoParsek where

import Eff
import Data.Monoid
import Control.DeepSeq (NFData(rnf))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
--import qualified Data.Attoparsec.ByteString.Buffer as B
--import qualified Data.Attoparsec.Text.Buffer as T

-- Temporary place holders
type BBuffer = ()
type TBuffer = ()

newtype Parser i a = Parser {
    unParser :: Eff (State (StateP i) (State Pos (State More (Error ErrMsg (IResult i))))) a
  } deriving (Functor, Monad)

newtype ErrMsg = ErrMsg String

instance Monoid ErrMsg where
  mempty = ErrMsg ""
  mappend (ErrMsg e1) (ErrMsg e2) = ErrMsg (e1 ++ e2)

{-
The full expanded attoparsec type:

newtype Parser i a = Parser {
      runParser :: forall r.
                   State i -> Pos -> More
                -> Failure i (State i)   r
                -> Success i (State i) a r
                -> IResult i r
    }

type Failure i t   r = t -> Pos -> More -> [String] -> String
                       -> IResult i r
type Success i t a r = t -> Pos -> More -> a -> IResult i r

Inlined version:

forall r.
   State i -> Pos -> More
-> (State i -> Pos -> More -> [String] -> String -> IResult i r)
-> (State i -> Pos -> More -> a -> IResult i r)
-> IResult i r

Rewrite to match the Eff structure better:

forall r.
   (a -> StateP i -> Pos -> More -> IResult i r)
-> (StateP i -> Pos -> More -> [String] -> String -> IResult i r)
-> StateP i -> Pos -> More
-> IResult i r


Eff (State (StateP i) (State Pos (State More (Error ErrMsg (IResult i)))))

-}

type family StateP i
type instance StateP ByteString = BBuffer
type instance StateP Text = TBuffer

data More = Complete | Incomplete
            deriving (Eq, Show)
newtype Pos = Pos { fromPos :: Int }
            deriving (Eq, Ord, Show, Num)

-- | The result of a parse.  This is parameterised over the type @i@
-- of string that was processed.
--
-- This type is an instance of 'Functor', where 'fmap' transforms the
-- value in a 'Done' result.
data IResult i r =
    Fail i [String] String
    -- ^ The parse failed.  The @i@ parameter is the input that had
    -- not yet been consumed when the failure occurred.  The
    -- @[@'String'@]@ is a list of contexts in which the error
    -- occurred.  The 'String' is the message describing the error, if
    -- any.
  | Partial (i -> IResult i r)
    -- ^ Supply this continuation with more input so that the parser
    -- can resume.  To indicate that no more input is available, pass
    -- an empty string to the continuation.
    --
    -- __Note__: if you get a 'Partial' result, do not call its
    -- continuation more than once.
  | Done i r
    -- ^ The parse succeeded.  The @i@ parameter is the input that had
    -- not yet been consumed (if any) when the parse succeeded.

instance (Show i, Show r) => Show (IResult i r) where
    show (Fail t stk msg) =
      unwords [ "Fail", show t, show stk, show msg]
    show (Partial _)          = "Partial _"
    show (Done t r)       = unwords ["Done", show t, show r]

instance (NFData i, NFData r) => NFData (IResult i r) where
    rnf (Fail t stk msg) = rnf t `seq` rnf stk `seq` rnf msg
    rnf (Partial _)  = ()
    rnf (Done t r)   = rnf t `seq` rnf r
    {-# INLINE rnf #-}

instance Functor (IResult i) where
    fmap _ (Fail t stk msg) = Fail t stk msg
    fmap f (Partial k)      = Partial (fmap f . k)
    fmap f (Done t r)   = Done t (f r)
