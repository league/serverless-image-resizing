import Crypto.Sign.Ed25519
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Base16 as Hex
import Data.String.Conversions (cs)
import System.Environment (getArgs)

main :: IO ()
main = do
  [secretKeyB64, message] <- getArgs
  let secretKey = SecretKey $ B64.decodeLenient $ cs secretKeyB64
      sig = unSignature $ dsign secretKey $ cs message
  putStrLn $ cs $ B64.encode $ sig
  putStrLn $ cs $ Hex.encode $ sig
