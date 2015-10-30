{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Text.Template

main :: IO ()
main = do
	t <- BS.readFile "sample1.tp"
	mr <- template convert get t
	maybe (return ()) BS.putStr mr

convert :: BS.ByteString -> [BS.ByteString]
convert "name" = ["Yoshikuni", "Kazuhiro"]
convert "i" = map (BSC.pack . show) [1 :: Int ..]
convert _ = []

get :: BS.ByteString -> IO [BS.ByteString]
get = mapM BS.readFile . (`map` ["1", "2"]) . (++) . BSC.unpack
