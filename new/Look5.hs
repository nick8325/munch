-- Adding the look 1-token lookahead feature to a parser.
{-# LANGUAGE TypeFamilies #-}
module Look5 where

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

{-# INLINE CONLIKE inBlocked #-}
inBlocked :: (p a -> p a) -> Result p a -> Result p a
inBlocked f (Blocked x) = Blocked (f x)
inBlocked _ x = x

{-# INLINE CONLIKE lookEta #-}
lookEta :: Parser p => Look p a -> Look p a
lookEta (Look p pr) = Look (eta p) (\t -> inBlocked eta (pr t))

{-# INLINE CONLIKE look #-}
look :: Parser p => (NextToken p -> Result p a) -> p a
look p = peek >>= execute . p

{-# INLINE CONLIKE execute #-}
execute :: Parser p => Result p a -> p a
execute (Ok x) = return x
execute Error = mzero
execute (Blocked p) = p

{-# INLINE CONLIKE fromParser #-}
fromParser :: Parser p => p a -> Look p a
fromParser p = Look p (\_ -> Blocked p)

{-# INLINE CONLIKE lookReturn #-}
lookReturn :: Parser p => a -> Look p a
lookReturn x = Look (return x) (\_ -> Ok x)

{-# INLINE CONLIKE lookBind #-}
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

{-# INLINE CONLIKE lookZero #-}
lookZero :: Parser p => Look p a
lookZero = Look mzero (\_ -> Error)

{-# INLINE CONLIKE lookChoice #-}
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

{-# INLINE CONLIKE lookPeek #-}
lookPeek :: Parser p => Look p (Maybe (Token (StreamType p)))
lookPeek = Look peek Ok

{-# INLINE CONLIKE lookGetInput #-}
lookGetInput :: Parser p => Look p (StreamType p)
lookGetInput = fromParser getInput

{-# INLINE CONLIKE lookPutInput #-}
lookPutInput :: Parser p => StreamType p -> Look p ()
lookPutInput inp = fromParser (putInput inp)

{-# INLINE CONLIKE lookSuccess #-}
lookSuccess :: Parser p => Look p a -> Look p a
lookSuccess p =
  p {
    parse = success (parse p),
    next = \t -> inBlocked success (next p t)
    }

{-# INLINE CONLIKE lookProgress #-}
lookProgress :: Parser p => Look p a -> Look p a
lookProgress p =
  p {
    parse = progress (parse p),
    next = \t ->
      case inBlocked progress (next p t) of 
        Ok _ -> Error
        x -> x
    }

{-# INLINE CONLIKE lookRun #-}
lookRun :: Parser p => Look p a -> StreamType p -> Maybe a
lookRun p inp = run (parse p) inp

instance Parser p => Functor (Look p) where
  {-# INLINE CONLIKE fmap #-}
  fmap f mx = do { x <- mx; return (f x) }
instance Parser p => Applicative (Look p) where
  {-# INLINE CONLIKE pure #-}
  pure x = return x
  {-# INLINE CONLIKE (<*>) #-}
  mf <*> mx = do { f <- mf; x <- mx; return (f x) }
  {-# INLINE CONLIKE (*>) #-}
  x *> y = x >> y
  {-# INLINE CONLIKE (<*) #-}
  mx <* my = do { x <- mx; my; return x }
instance Parser p => Alternative (Look p) where
  {-# INLINE CONLIKE empty #-}
  empty = mzero
  {-# INLINE CONLIKE (<|>) #-}
  x <|> y = x `mplus` y
  {-# INLINE CONLIKE some #-}
  some p = parseSome p
  {-# INLINE CONLIKE many #-}
  many p = parseMany p
instance Parser p => Monad (Look p) where
  {-# INLINE CONLIKE return #-}
  return x = lookReturn x
  {-# INLINE CONLIKE (>>=) #-}
  x >>= f = x `lookBind` f
  {-# INLINE CONLIKE fail #-}
  fail _ = lookZero
instance Parser p => MonadPlus (Look p) where
  {-# INLINE CONLIKE mzero #-}
  mzero = lookZero
  {-# INLINE CONLIKE mplus #-}
  x `mplus` y = x `lookChoice` y
instance Parser p => Parser (Look p) where
  type StreamType (Look p) = StreamType p
  {-# INLINE CONLIKE peek #-}
  peek = lookPeek
  {-# INLINE CONLIKE getInput #-}
  getInput = lookGetInput
  {-# INLINE CONLIKE putInput #-}
  putInput x = lookPutInput x
  {-# INLINE CONLIKE success #-}
  success p = lookSuccess p
  {-# INLINE CONLIKE progress #-}
  progress p = lookProgress p
  {-# INLINE CONLIKE run #-}
  run p s = lookRun p s
  {-# INLINE CONLIKE eta #-}
  eta = lookEta