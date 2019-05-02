{-# LANGUAGE PackageImports     #-}

module Hashing where

import           "cryptonite" Crypto.Hash
                 (Context, Digest, SHA256, hash, hashFinalize, hashInit,
                 hashUpdate)
import           Pipes                    ((<-<), (<~), (>->), (>~), (~<), (~>))
import qualified Pipes                    as P
import qualified Pipes.Prelude            as PP
import           Control.Monad.ST         (ST)

data Chunk = EOF | Chunk !BS.ByteString

foo :: (Monad m) => Context SHA256 -> P.Pipe Chunk (Digest SHA256) m r
foo ctx = do
  chunk <- P.await
  case chunk of
    EOF -> do
      P.yield (hashFinalize ctx)
      bar
    Chunk bs ->
      foo (hashUpdate ctx bs)

bar :: (Monad m) => P.Pipe Chunk (Digest SHA256) m r
bar = do
  let
    ctx = hashInit
  foo ctx
  bar

baz :: IO ()
baz =
  P.runEffect ((P.yield (Chunk "foobar") >> P.yield (Chunk "lmao") >> P.yield EOF) >-> bar >-> PP.print)

qux :: IO ()
qux =
  print $ show (hash ("foobarlmao"::BS.ByteString) :: Digest SHA256)

type Base32 = LBS.ByteString
parseHash32 :: LBS.ByteString -> ST s Base32
parseHash32 input = do
  let
    len = 32
    base32Chars :: LBS.ByteString
    base32Chars = "0123456789abcdfghijklmnpqrsvwxyz"
  when (LBS.length input /= len) $ fail "wrong hash length"
  forM_ [ 0 .. len - 1 ] $ \n -> do
    let
      c = LBS.index input n
      maybeIdx = LBS.elemIndex c base32Chars
      b = n * 5
      i = div b 8
      j = mod b 8
      -- i is the byte position
      -- j is the bit offset
      -- hash[i] = idx << j

    trace (show maybeIdx) return undefined
  return undefined
