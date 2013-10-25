-- Adding the look 1-token lookahead feature to a parser.
{-# LANGUAGE TypeFamilies #-}
module Look4 where

import Class
import Stream
import InstanceHelpers
import Control.Applicative
import Control.Monad

data Look p a = Look {
  parse :: p a,
  process :: Process p a
  }

type Process p a = Blocking (Peek p a) a
type Peek p a = Maybe (Token (StreamType p)) -> Result p a
type Result p a = Blocking (p a) a

data Blocking a b =
    Ok b
  | Error
  | Block a

{-# INLINE CONLIKE lookEta #-}
lookEta :: Parser p => Look p a -> Look p a
lookEta (Look p pr) = Look (eta p) (inBlock (\f t -> inBlock eta (f t)) pr)

{-# INLINE CONLIKE flatten #-}
flatten :: (a -> Blocking b c) -> Blocking a c -> Blocking b c
flatten f (Block x) = f x
flatten f (Ok x) = Ok x
flatten f Error = Error

{-# INLINE CONLIKE inBlock #-}
inBlock :: (a -> a) -> Blocking a b -> Blocking a b
inBlock f = flatten (Block . f)

{-# INLINE CONLIKE inject #-}
inject :: MonadPlus m => (a -> m b) -> Blocking a b -> m b
inject f (Ok x) = return x
inject f Error = mzero
inject f (Block x) = f x

{-# INLINE CONLIKE result #-}
result :: Parser p => Result p a -> p a
result = inject id

{-# INLINE CONLIKE feed #-}
feed :: Parser p => Maybe (Token (StreamType p)) -> Look p a -> Result p a
feed t = flatten (\f -> f t) . process

{-# INLINE CONLIKE fromParser #-}
fromParser :: Parser p => p a -> Look p a
fromParser p = Look p (Block (\_ -> Block p))

{-# INLINE CONLIKE fromProcess #-}
fromProcess :: Parser p => Process p a -> Look p a
fromProcess p = Look (inject (\f -> peek >>= result . f) p) p

{-# INLINE CONLIKE lookReturn #-}
lookReturn :: Parser p => a -> Look p a
lookReturn x = fromProcess (Ok x)

{-# INLINE CONLIKE lookBind #-}
lookBind :: Parser p => Look p a -> (a -> Look p b) -> Look p b
x `lookBind` f =
  Look {
    parse = parse x >>= parse . f,
    process = process x `processBind` f
    }

{-# INLINE CONLIKE processBind #-}
processBind :: Parser p => Process p a -> (a -> Look p b) -> Process p b
Ok x `processBind` f = process (f x)
Error `processBind` f = Error
Block x `processBind` f =
  Block $ \t ->
    case x t of
      Ok x -> feed t (f x)
      Error -> Error
      Block p -> Block (p >>= parse . f)

{-# INLINE CONLIKE lookZero #-}
lookZero :: Parser p => Look p a
lookZero = fromProcess Error

{-# INLINE CONLIKE lookChoice #-}
lookChoice :: Parser p => Look p a -> Look p a -> Look p a
p `lookChoice` q = makeChoice process p q choice
  where
    choice = fromProcess (Block (\t -> feed t p `resultChoice` feed t q))

{-# INLINE CONLIKE resultChoice #-}
resultChoice :: Parser p => Result p a -> Result p a -> Result p a
x `resultChoice` y = makeChoice id x y (Block (result x `mplus` result y))

{-# INLINE CONLIKE makeChoice #-}
makeChoice :: (a -> Blocking b c) -> a -> a -> a -> a
makeChoice pred p q both =
  case (pred p, pred q) of
    (Ok _, _) -> p
    (_, Error) -> p
    (Error, _) -> q
    _ -> both

{-# INLINE CONLIKE lookPeek #-}
lookPeek :: Parser p => Look p (Maybe (Token (StreamType p)))
lookPeek = fromProcess (Block Ok)

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
    process = inBlock (\f t -> inBlock success (f t)) (process p)
    }

{-# INLINE CONLIKE lookProgress #-}
lookProgress :: Parser p => Look p a -> Look p a
lookProgress p =
  p {
    parse = progress (parse p),
    process = blockingProgress (inBlock (\f t -> blockingProgress (inBlock progress (f t))) (process p))
    }

{-# INLINE CONLIKE blockingProgress #-}
blockingProgress :: Blocking a b -> Blocking a b
blockingProgress (Block x) = Block x
blockingProgress _ = Error

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