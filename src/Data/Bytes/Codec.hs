{-# LANGUAGE RankNTypes #-}
module Data.Bytes.Codec where

import Data.Word

import Data.Codec
import Data.Bytes.Get
import Data.Bytes.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

type BinaryCodec a = forall get put. (MonadGet get, MonadPut put) => Codec get put a

byteString :: Int -> BinaryCodec ByteString
byteString n = Codec
  { parse = getBytes n
  , produce = \bs -> if BS.length bs == n
      then putByteString bs *> pure bs
      else fail "ByteString wrong size for field."
  }

word8 :: BinaryCodec Word8
word8 = codec getWord8 putWord8

word16be :: BinaryCodec Word16
word16be = codec getWord16be putWord16be

word16le :: BinaryCodec Word16
word16le = codec getWord16le putWord16le

word16host :: BinaryCodec Word16
word16host = codec getWord16host putWord16host

word32be :: BinaryCodec Word32
word32be = codec getWord32be putWord32be

word32le :: BinaryCodec Word32
word32le = codec getWord32le putWord32le

word32host :: BinaryCodec Word32
word32host = codec getWord32host putWord32host

word64be :: BinaryCodec Word64
word64be = codec getWord64be putWord64be

word64le :: BinaryCodec Word64
word64le = codec getWord64le putWord64le

word64host :: BinaryCodec Word64
word64host = codec getWord64host putWord64host

wordhost :: BinaryCodec Word
wordhost = codec getWordhost putWordhost
