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

data Result p a  =
    Ok a
  | Error (Int -> [Error] -> [Error])
  | Blocked (p a)

{-# INLINE inBlocked #-}
inBlocked :: (p a -> p a) -> Result p a -> Result p a
inBlocked f (Blocked x) = Blocked (f x)
inBlocked _ x = x

{-# INLINE execute #-}
execute :: Parser p => Result p a -> p a
execute (Ok x) = return x
execute (Error f) = expected f mzero
execute (Blocked p) = p

{-# INLINE fromParser #-}
fromParser :: Parser p => p a -> Look p a
fromParser p = look p (\_ -> Blocked p)

{-# INLINE lookReturn #-}
lookReturn :: Parser p => a -> Look p a
lookReturn x = look (return x) (\_ -> Ok x)

{-# INLINE lookBind #-}
lookBind :: Parser p => Look p a -> (a -> Look p b) -> Look p b
x `lookBind` f =
  look
    (parse x >>= parse . f)
    (\t ->
      case next x t of
        Ok y -> next (f y) t
        Error f -> Error f
        Blocked p -> Blocked (p >>= parse . f))

{-# INLINE lookZero #-}
lookZero :: Parser p => Look p a
lookZero = look mzero (\_ -> Error (\_ -> id))

{-# INLINE lookChoice #-}
lookChoice :: Parser p => Look p a -> Look p a -> Look p a
p `lookChoice` q =
  look
    (peek >>= \t -> execute (next p t `resultChoice` next q t))
    (\t -> next p t `resultChoice` next q t)
  where
    {-# INLINE resultChoice #-}
    resultChoice (Ok x) _ = Ok x
    resultChoice (Error f) (Ok x) = Ok x
    resultChoice (Error f) (Error g) = Error (\n -> f n . g n)
    resultChoice (Error f) (Blocked p) = Blocked (expected f p)
    resultChoice (Blocked p) (Error f) = Blocked (expected f p)
    resultChoice p q = Blocked (execute p `mplus` execute q)

{-# INLINE lookPeek #-}
lookPeek :: Parser p => Look p (Maybe (Token (StreamType p)))
lookPeek = look peek Ok

{-# INLINE lookGetInput #-}
lookGetInput :: Parser p => Look p (StreamType p)
lookGetInput = fromParser getInput

{-# INLINE lookPutInput #-}
lookPutInput :: Parser p => StreamType p -> Look p ()
lookPutInput inp = fromParser (putInput inp)

{-# INLINE lookNote #-}
lookNote :: Parser p => Look p a -> String -> Look p a
lookNote p x =
  look
    (parse p <?> x)
    (\t ->
      case next p t of
        Ok x -> Ok x
        Error f -> Error (\n errs -> Expected n x:f n errs)
        Blocked p -> Blocked (p <?> x))

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
        Ok _ -> Error (\_ _ -> [])
        x -> x)

{-# INLINE lookRun #-}
lookRun :: Parser p => Look p a -> StreamType p -> Class.Result a
lookRun p inp = run (parse p) inp

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
  (<?>) = lookNote
  success = lookSuccess
  progress = lookProgress
  run = lookRun