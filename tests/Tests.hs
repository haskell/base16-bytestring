import Data.Char (ord)
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base16.Lazy as Base16L

-- Three-line "test framework" for Haskell.  Crude, but at least it tells you
-- the line number of the failure without you having to specify it.
shouldBe :: (Eq a, Show a) => a -> a -> IO Bool
shouldBe a b = return (a == b)
infix 0 `shouldBe`

c2w :: Char -> Word8
c2w = fromIntegral . ord

main :: IO ()
main = do
    let hexL  = map c2w "0123456789abcdef"
        hexU  = map c2w "0123456789ABCDEF"
        hexUL = map c2w "0123456789ABCDEFabcdef"
        notHex = [c | c <- [0..255], c `notElem` hexUL]
        hexL2 = do a <- hexL; b <- hexL; [a,b]
        hexU2 = do a <- hexU; b <- hexU; [a,b]
        bytes = B.pack [0..255]

    -- Encode every byte
    True <- Base16.encode bytes `shouldBe` B.pack hexL2

    -- Decode every valid hex pair
    True <- Base16.decode (B.pack hexL2) `shouldBe` (bytes, B.empty)
    True <- Base16.decode (B.pack hexU2) `shouldBe` (bytes, B.empty)

    -- Decode every invalid byte paired with a correct byte
    let bads1 = [B.pack [a,b] | a <- notHex, b <- hexUL]
    let bads2 = [B.pack [a,b] | a <- hexUL, b <- notHex]
    True <- map Base16.decode bads1 `shouldBe` map (\s -> (B.empty, s)) bads1
    True <- map Base16.decode bads2 `shouldBe` map (\s -> (B.empty, s)) bads2

    -- Like above, but start with a correct byte
    let correctHex   = B.pack [97,98]
        correctBytes = B.pack [171]
    True <- map (Base16.decode . (correctHex `B.append`)) bads1
            `shouldBe` map (\s -> (correctBytes, s)) bads1
    True <- map (Base16.decode . (correctHex `B.append`)) bads2
            `shouldBe` map (\s -> (correctBytes, s)) bads2

    -- Like above, but end with a correct byte
    True <- map (Base16.decode . (`B.append` correctHex)) bads1
            `shouldBe` map (\s -> (B.empty, s `B.append` correctHex)) bads1
    True <- map (Base16.decode . (`B.append` correctHex)) bads2
            `shouldBe` map (\s -> (B.empty, s `B.append` correctHex)) bads2

    -- Lazy decoding also works with odd length chunks
    let encodedLazy = BL.fromChunks $ map (B.pack . map c2w) ["614","239","6","142","39"]
    True <- Base16L.decode encodedLazy `shouldBe` (BL.pack . map c2w $ "aB9aB9",BL.empty)

    -- Lazy decoding is lazy on success
    let encodedLazy = BL.iterate id 48
    True <- (BL.unpack . BL.take 8 . fst . Base16L.decode $ encodedLazy)
            `shouldBe` [0,0,0,0,0,0,0,0]

    -- Lazy decoding is lazy on failure
    let encodedLazy = BL.iterate id 47
    True <- (BL.unpack . BL.take 8 . fst . Base16L.decode $ encodedLazy)
            `shouldBe` []

    return ()
