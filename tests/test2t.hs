{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Template

main :: IO ()
main = do
	t <- T.readFile "samples/sample1.tp"
	mr <- template convert get t
	maybe (return ()) T.putStr mr

convert :: T.Text -> [T.Text]
convert "name" = ["Yoshikuni", "Kazuhiro"]
convert "i" = map (T.pack . show) [1 :: Int ..]
convert _ = []

get :: T.Text -> IO [T.Text]
get = mapM T.readFile
	. (`map` ["1", "2"]) . (++) . ("samples/" ++) . T.unpack
