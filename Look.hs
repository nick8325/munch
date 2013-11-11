-- Adding the look 1-token lookahead feature to a parser.
{-# LANGUAGE TypeFamilies #-}
module Look where

import Class hiding (Result(..))
import qualified Class
import Stream
import InstanceHelpers
import Control.Applicative
import Control.Monad

data Look p a = Look {
  parse :: p a,
  next :: NextToken p -> Result p a
  }

{-# INLINE look #-}
look = Look

type NextToken p = Maybe (Token (StreamType p))

data Result p a = Result ([String] -> [String]) (Result1 p a)
data Result1 p a  = Ok a | Error | Blocked (p a)

{-# INLINE inBlocked #-}
inBlocked :: (p a -> p a) -> Result p a -> Result p a
inBlocked f (Result g (Blocked x)) = Result g (Blocked (f x))
inBlocked _ x = x

{-# INLINE execute #-}
execute :: Parser p => Result p a -> p a
-- XXX see TODO
execute (Result f (Ok x)) = expected f mzero `mplus` return x
execute (Result f x) = expected f (execute1 x)

{-# INLINE execute1 #-}
execute1 :: Parser p => Result1 p a -> p a
execute1 (Ok x) = return x
execute1 Error = mzero
execute1 (Blocked p) = p

{-# INLINE fromParser #-}
fromParser :: Parser p => p a -> Look p a
fromParser p = look p (\_ -> Result id (Blocked p))

{-# INLINE lookReturn #-}
lookReturn :: Parser p => a -> Look p a
lookReturn x = look (return x) (\_ -> Result id (Ok x))

{-# INLINE lookBind #-}
lookBind :: Parser p => Look p a -> (a -> Look p b) -> Look p b
x `lookBind` f =
  look
    (parse x >>= parse . f)
    (\t ->
      case next x t of
        Result g (Ok y) ->
          case next (f y) t of
            Result h z -> Result (g . h) z
        Result f Error -> Result f Error
        Result g (Blocked p) -> Result g (Blocked (p >>= parse . f)))

{-# INLINE lookZero #-}
lookZero :: Parser p => Look p a
lookZero = look mzero (\_ -> Result id Error)

{-# INLINE lookChoice #-}
lookChoice :: Parser p => Look p a -> Look p a -> Look p a
p `lookChoice` q =
  look
    (peek >>= \t -> execute (next p t `resultChoice` next q t))
    (\t -> next p t `resultChoice` next q t)
  where
    {-# INLINE resultChoice #-}
    resultChoice (Result f x) (Result g y) = Result (f . g) (result1Choice x y)
    {-# INLINE result1Choice #-}
    result1Choice (Ok x) _ = Ok x
    result1Choice Error x = x
    result1Choice x Error = x
    result1Choice p q = Blocked (execute1 p `mplus` execute1 q)

{-# INLINE lookPeek #-}
lookPeek :: Parser p => Look p (Maybe (Token (StreamType p)))
lookPeek = look peek (Result id . Ok)

{-# INLINE lookGetInput #-}
lookGetInput :: Parser p => Look p (StreamType p)
lookGetInput = fromParser getInput

{-# INLINE lookPutInput #-}
lookPutInput :: Parser p => StreamType p -> Look p ()
lookPutInput inp = fromParser (putInput inp)

{-# INLINE lookExpected #-}
lookExpected :: Parser p => ([String] -> [String]) -> Look p a -> Look p a
lookExpected f p =
  look
    (expected f (parse p))
    (\t ->
      case next p t of
        Result g y -> Result (f . g) y)

{-# INLINE lookSuccess #-}
lookSuccess :: Parser p => Look p a -> Look p a
lookSuccess p =
  look
    (success (parse p))
    (\t -> inBlocked success (next p t))

{-# INLINE lookProgress #-}
lookProgress :: Parser p => Look p a -> Look p a
lookProgress p =
  look
    (progress (parse p))
    (\t ->
      case inBlocked progress (next p t) of
        Result f (Ok _) -> Result f Error
        x -> x)

{-# INLINE lookRun #-}
lookRun :: Parser p => Look p a -> StreamType p -> Class.Result a
lookRun p inp = run (parse p) inp

{-# INLINE lookMunch #-}
lookMunch :: Parser p => Look p ()
lookMunch = fromParser munch

instance Parser p => Functor (Look p) where
  fmap = liftM
instance Parser p => Applicative (Look p) where
  pure = return
  (<*>) = ap
  (*>) = (>>)
  (<*) = followedBy
instance Parser p => Alternative (Look p) where
  empty = mzero
  (<|>) = mplus
  some = parseSome
  many = parseMany
instance Parser p => Monad (Look p) where
  return = lookReturn
  (>>=) = lookBind
  fail _ = lookZero
instance Parser p => MonadPlus (Look p) where
  mzero = lookZero
  mplus = lookChoice
instance Parser p => Parser (Look p) where
  type StreamType (Look p) = StreamType p
  peek = lookPeek
  getInput = lookGetInput
  putInput = lookPutInput
  expected = lookExpected
  success = lookSuccess
  progress = lookProgress
  run = lookRun
  munch = lookMunch