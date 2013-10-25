-- Adding the look 1-token lookahead feature to a parser.
{-# LANGUAGE TypeFamilies #-}
module Look where

import Class
import Stream
import InstanceHelpers
import Control.Applicative
import Control.Monad

data Look p a = Look {
  parse :: p a,
  next :: NextToken p -> Result p a
  }

type NextToken p = Maybe (Token (StreamType p))

data Result p a  =
    Ok a
  | Error
  | Blocked (p a)

{-# INLINE inBlocked #-}
inBlocked :: (p a -> p a) -> Result p a -> Result p a
inBlocked f (Blocked x) = Blocked (f x)
inBlocked _ x = x

{-# INLINE look #-}
look :: Parser p => (NextToken p -> Result p a) -> p a
look p = peek >>= execute . p

{-# INLINE execute #-}
execute :: Parser p => Result p a -> p a
execute (Ok x) = return x
execute Error = mzero
execute (Blocked p) = p

{-# INLINE fromParser #-}
fromParser :: Parser p => p a -> Look p a
fromParser p = Look p (\_ -> Blocked p)

{-# INLINE lookReturn #-}
lookReturn :: Parser p => a -> Look p a
lookReturn x = Look (return x) (\_ -> Ok x)

{-# INLINE lookBind #-}
lookBind :: Parser p => Look p a -> (a -> Look p b) -> Look p b
x `lookBind` f =
  Look {
    parse = parse x >>= parse . f,
    next = \t ->
      case next x t of
        Ok y -> next (f y) t
        Error -> Error
        Blocked p -> Blocked (p >>= parse . f)
    }

{-# INLINE lookZero #-}
lookZero :: Parser p => Look p a
lookZero = Look mzero (\_ -> Error)

{-# INLINE lookChoice #-}
lookChoice :: Parser p => Look p a -> Look p a -> Look p a
p `lookChoice` q =
  Look {
    parse = look (\t -> next p t `resultChoice` next q t),
    next = \t -> next p t `resultChoice` next q t
    }
  where
    {-# INLINE resultChoice #-}
    resultChoice (Ok x) _ = Ok x
    resultChoice Error x = x
    resultChoice x Error = x
    resultChoice p q = Blocked (execute p `mplus` execute q)

{-# INLINE lookPeek #-}
lookPeek :: Parser p => Look p (Maybe (Token (StreamType p)))
lookPeek = Look peek Ok

{-# INLINE lookGetInput #-}
lookGetInput :: Parser p => Look p (StreamType p)
lookGetInput = fromParser getInput

{-# INLINE lookPutInput #-}
lookPutInput :: Parser p => StreamType p -> Look p ()
lookPutInput inp = fromParser (putInput inp)

{-# INLINE lookSuccess #-}
lookSuccess :: Parser p => Look p a -> Look p a
lookSuccess p =
  p {
    parse = success (parse p),
    next = \t -> inBlocked success (next p t)
    }

{-# INLINE lookProgress #-}
lookProgress :: Parser p => Look p a -> Look p a
lookProgress p =
  p {
    parse = progress (parse p),
    next = \t ->
      case inBlocked progress (next p t) of 
        Ok _ -> Error
        x -> x
    }

{-# INLINE lookRun #-}
lookRun :: Parser p => Look p a -> StreamType p -> Maybe a
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
  success = lookSuccess
  progress = lookProgress
  run = lookRun