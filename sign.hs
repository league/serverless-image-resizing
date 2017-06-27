import Crypto.Sign.Ed25519
import qualified Data.ByteString.Base64.URL as B64
import Data.String.Conversions (cs)
import System.Environment (getArgs)

main :: IO ()
main = do
  [secretKeyB64, message] <- getArgs
  let secretKey = SecretKey $ B64.decodeLenient $ cs secretKeyB64
      sig = dsign secretKey $ cs message
  putStrLn $ cs $ B64.encode $ unSignature sig
