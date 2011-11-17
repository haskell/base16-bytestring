import Criterion.Main
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as B

generate :: Int -> B.ByteString
generate n = B.pack . take n . cycle $ [0..255]

main = defaultMain [
         bgroup "encode" [
           bench "8" $ whnf B16.encode (generate 8)
         , bench "32" $ whnf B16.encode (generate 32)
         , bench "128" $ whnf B16.encode (generate 128)
         , bench "1024" $ whnf B16.encode (generate 1024)
         , bench "65536" $ whnf B16.encode (generate 65536)
         ]
       , bgroup "decode" [
           bench "8" $ whnf (B16.decode . B16.encode) (generate 8)
         , bench "32" $ whnf (B16.decode . B16.encode) (generate 32)
         , bench "128" $ whnf (B16.decode . B16.encode) (generate 128)
         , bench "1024" $ whnf (B16.decode . B16.encode) (generate 1024)
         , bench "65536" $ whnf (B16.decode . B16.encode) (generate 65536)
         ]
       ]
