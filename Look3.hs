-- Adding the look 1-token lookahead feature to a parser.
{-# LANGUAGE TypeFamilies #-}
module Look3 where

import Class
import Stream
import InstanceHelpers
import Control.Applicative
import Control.Monad

data Look p a = Look {
  parse :: p a,
  process :: Maybe (Process p a)
  }

data Process p a =
    Peek (Maybe (Token (StreamType p)) -> Result p a)
  | HereOk a
  | HereError

data Result p a =
    Ok a
  | Error
  | Block (p a)

{-# INLINE CONLIKE lookEta #-}
lookEta :: Parser p => Look p a -> Look p a
lookEta (Look p pr) = Look (eta p) (fmap processEta pr)

{-# INLINE CONLIKE processEta #-}
processEta :: Parser p => Process p a -> Process p a
processEta (Peek f) = Peek (\t -> resultEta (f t))
processEta x = x

{-# INLINE CONLIKE resultEta #-}
resultEta :: Parser p => Result p a -> Result p a
resultEta (Ok x) = Ok x
resultEta Error = Error
resultEta (Block p) = Block (eta p)

{-# INLINE CONLIKE feed #-}
feed :: Parser p => Maybe (Token (StreamType p)) -> Look p a -> Result p a
feed _ Look { parse = p, process = Nothing } = Block p
feed t Look { process = Just (Peek f) } = f t
feed _ Look { process = Just (HereOk x) } = Ok x
feed _ Look { process = Just HereError } = Error

{-# INLINE CONLIKE activate #-}
activate :: Parser p => Process p a -> p a
activate (HereOk x) = return x
activate HereError = mzero
activate (Peek f) = peek >>= result . f

{-# INLINE CONLIKE result #-}
result :: Parser p => Result p a -> p a
result (Ok x) = return x
result Error = mzero
result (Block p) = p

{-# INLINE CONLIKE fromProcess #-}
fromProcess :: Parser p => Process p a -> Look p a
fromProcess p = Look (activate p) (Just p)

{-# INLINE CONLIKE fromParser #-}
fromParser :: Parser p => p a -> Look p a
fromParser p = Look p Nothing

{-# INLINE CONLIKE lookReturn #-}
lookReturn :: Parser p => a -> Look p a
lookReturn x = fromProcess (HereOk x)

{-# INLINE CONLIKE lookBind #-}
lookBind :: Parser p => Look p a -> (a -> Look p b) -> Look p b
x `lookBind` f =
  Look {
    parse = parse x >>= parse . f,
    process = process x `processBind` f
    }

{-# INLINE CONLIKE processBind #-}
processBind :: Parser p => Maybe (Process p a) -> (a -> Look p b) -> Maybe (Process p b)
Nothing `processBind` f = Nothing
Just (HereOk x) `processBind` f = process (f x)
Just HereError `processBind` f = Just HereError
Just (Peek x) `processBind` f =
  Just . Peek $ \t ->
    case x t of
      Ok x -> feed t (f x)
      Error -> Error
      Block p -> Block (p >>= parse . f)

{-# INLINE CONLIKE lookZero #-}
lookZero :: Parser p => Look p a
lookZero = fromProcess HereError

{-# INLINE CONLIKE lookChoice #-}
lookChoice :: Parser p => Look p a -> Look p a -> Look p a
p `lookChoice` q =
  case (process p, process q) of
    (Just (HereOk _), _) -> p
    (Just HereError, _) -> q
    (_, Just HereError) -> p
    _ -> fromProcess (Peek (\t -> feed t p `resultChoice` feed t q))

{-# INLINE resultChoice #-}
resultChoice :: Parser p => Result p a -> Result p a -> Result p a
Ok x `resultChoice` _ = Ok x
Error `resultChoice` x = x
x `resultChoice` Error = x
x `resultChoice` y = Block (result x `mplus` result y)

{-# INLINE CONLIKE lookPeek #-}
lookPeek :: Parser p => Look p (Maybe (Token (StreamType p)))
lookPeek = fromProcess (Peek Ok)

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
    process = fmap processSuccess (process p)
    }

{-# INLINE CONLIKE processSuccess #-}
processSuccess :: Parser p => Process p a -> Process p a
processSuccess (Peek f) = Peek (\t -> resultSuccess (f t))
processSuccess x = x

{-# INLINE CONLIKE resultSuccess #-}
resultSuccess :: Parser p => Result p a -> Result p a
resultSuccess (Block p) = Block (success p)
resultSuccess x = x

{-# INLINE CONLIKE lookProgress #-}
lookProgress :: Parser p => Look p a -> Look p a
lookProgress p =
  p {
    parse = progress (parse p),
    process = fmap processProgress (process p)
    }

{-# INLINE CONLIKE processProgress #-}
processProgress :: Parser p => Process p a -> Process p a
processProgress (Peek f) = Peek (\t -> resultProgress (f t))
processProgress _ = HereError

{-# INLINE CONLIKE resultProgress #-}
resultProgress :: Parser p => Result p a -> Result p a
resultProgress (Block p) = Block (progress p)
resultProgress _ = Error

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