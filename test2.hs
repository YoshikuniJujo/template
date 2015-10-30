import Text.Template.String

main :: IO ()
main = do
	t <- readFile "sample1.tp"
	mr <- template convert get t
	maybe (return ()) putStr mr

convert :: String -> [String]
convert "name" = ["Yoshikuni", "Kazuhiro"]
convert "i" = map show [1 :: Int ..]
convert _ = []

get :: String -> IO [String]
get = mapM readFile . (`map` ["1", "2"]) . (++)
