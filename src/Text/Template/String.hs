module Text.Template.String (template) where

import Prelude hiding (mapM)
import Control.Applicative
import Control.Arrow
import Data.Traversable
import Data.Maybe
import Data.Char

template :: (Applicative m, Monad m) =>
	(String -> [String]) -> (String -> m [String]) -> String -> m (Maybe String)
template c g t = listToMaybe . getZipList
	<$> maybe (return $ ZipList [])
		(templateS (ZipList . c) ((ZipList <$>) . g)) (syntax t)

templateS :: (Applicative m, Monad m) =>
	(String -> ZipList String) -> (String -> m (ZipList String)) -> Syntax ->
	m (ZipList String)
templateS cnv _get (Var v) = return $ cnv v
templateS _cnv get (Get g) = get g
templateS _cnv _get (Str s) = return $ pure s -- return $ ZipList [s]
templateS cnv get (If (vl1, vl2) th el) = do
	val1 <- templateSs cnv get vl1
	val2 <- templateSs cnv get vl2
	ts <- templateSs cnv get th
	es <- templateSs cnv get el
	return $ (\v1 v2 t e -> if v1 == v2 then t else e)
		<$> val1 <*> val2 <*> ts <*> es
--	error $ show val1 ++ " " ++ show val2
--	error $ show val1
--	if val1 == val2
--		then templateSs cnv get t
--		else templateSs cnv get e
templateS cnv get (List ss) = ZipList . (: []) . concat . getZipList
	<$> ((concat <$>) . sequenceA) <$> mapM (templateS cnv get) ss
templateS _cnv _get (Plain p) = return $ pure p
-- templateS _cnv _get (Plain p) = return $ ZipList [p]

templateSs :: (Applicative m, Monad m) =>
	(String -> ZipList String) -> (String -> m (ZipList String)) ->
	[Syntax] -> m (ZipList String)
templateSs cnv get ss = ((concat <$>) . sequenceA) <$> mapM (templateS cnv get) ss

data Syntax
	= Var String | Get String | Str String
	| If ([Syntax], [Syntax]) [Syntax] [Syntax]
	| List [Syntax]
	| Plain String
	deriving Show

syntax :: String -> Maybe Syntax
syntax s = case parses . processIf $ tokens s of
	(ss, []) -> Just $ List ss
	_ -> Nothing

parse :: [Token] -> Maybe (Syntax, [Token])
parse (TVar v : ts) = Just (Var v , ts)
parse (TGet g : ts) = Just (Get g , ts)
parse (TStr s : ts) = Just (Str s , ts)
parse (TIf : ts) = case parses ts of
	(v1, TEq : ts2) -> case parses ts2 of
		(v2, TThen : ts3) -> case parses ts3 of
			(t, TElse : ts4) -> case parses ts4 of
				(e, TEnd : ts5) -> Just (If (v1, v2) t e, ts5)
				_ -> Nothing
			_ -> Nothing
		_ -> Nothing
	_ -> Nothing
{-
parse (TIf : TVar v : TEq : TStr s : TThen : ts) = case parses ts of
	(t, TElse : ts') -> case parses ts' of
		(e, TEnd : ts'') -> Just (If (Left v, s) t e, ts'')
		_ -> Nothing
	_ -> Nothing
parse (TIf : TGet v : TEq : TStr s : TThen : ts) = case parses ts of
	(t, TElse : ts') -> case parses ts' of
		(e, TEnd : ts'') -> Just (If (Right v, s) t e, ts'')
		_ -> Nothing
	_ -> Nothing
	-}
parse (TOpen : ts) = case parses ts of
	(ss, TClose : ts') -> Just (List ss, ts')
	_ -> Nothing
parse (TPlain p : ts) = Just (Plain p , ts)
parse _ = Nothing

parses :: [Token] -> ([Syntax], [Token])
parses ts = case parse ts of
	Just (s, ts') -> (s :) `first` parses ts'
	_ -> ([], ts)

processIf :: [Token] -> [Token]
processIf (TIf : ts) = TIf : uncurry (++) ((filter (not . isEmpty) `first`) $ span (/= TThen) ts)
processIf (t : ts) = t : processIf ts
processIf _ = []

data Token
	= TVar String | TGet String | TStr String
	| TIf | TEq | TThen | TElse | TEnd
	| TOpen | TClose
	| TPlain String
	deriving (Show, Eq)

isEmpty :: Token -> Bool
isEmpty (TPlain p) = all isSpace p
isEmpty _ = False

tokens :: String -> [Token]
tokens "" = []
tokens ('\\' : 'i' : 'f' : s) = TIf : tokens (dropWhile isSpace s)
tokens ('\\' : '=' : '=' : s) = TEq : tokens (dropWhile isSpace s)
tokens ('\\' : 't' : 'h' : 'e' : 'n' : '\n' : s) = TThen : tokens s
tokens ('\\' : 'e' : 'l' : 's' : 'e' : '\n' : s) = TElse : tokens s
tokens ('\\' : 'e' : 'n' : 'd' : '\n' : s) = TEnd : tokens s
tokens ('\\' : '[' : '\n' : s) = TOpen : tokens s
tokens ('\\' : ']' : '\n' : s) = TClose : tokens s
tokens ('\\' : c : s) = TPlain [c] : tokens s
tokens ('$' : '{' : '"' : s) =
	uncurry (:) . (TStr . init *** tokens . tail) $ span (/= '}') s
tokens ('$' : '{' : s) =
	uncurry (:) . (TVar *** tokens . tail) $ span (/= '}') s
tokens ('@' : '{' : s) =
	uncurry (:) . (TGet *** tokens . tail) $ span (/= '}') s
tokens s = case span (`notElem` "$@\\") s of
	(p, s') -> TPlain p : tokens s'
