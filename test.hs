import Text.Template

main :: IO ()
main = do
	t <- readFile "sample2.tp"
	mr <- template convert get t
	maybe (return ()) putStr mr

convert :: String -> [String]
convert "name" = ["Yoshikuni"]
convert "i" = map show [1 :: Int ..]
convert _ = []

get :: String -> IO [String]
get = mapM readFile . (`map` ["1", "2"]) . (++)
