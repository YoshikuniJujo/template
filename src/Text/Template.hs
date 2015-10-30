{-# LANGUAGE OverloadedStrings, PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Text.Template (template, StringLike, StringLikeList) where

import Prelude hiding (mapM)
import Control.Applicative
import Control.Arrow
import Data.Monoid
import Data.Traversable
import Data.Maybe
import Data.Char
import Data.String

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

template :: (Applicative m, Monad m, StringLike s, IsString s) =>
	(s -> [s]) -> (s -> m [s]) -> s -> m (Maybe s)
template c g t = listToMaybe . getZipList
	<$> maybe (return $ ZipList [])
		(templateS (ZipList . c) ((ZipList <$>) . g)) (checkInf <$> syntax t)

templateS :: (Applicative m, Monad m, Monoid s, Eq s) =>
	(s -> ZipList s) -> (s -> m (ZipList s)) -> Syntax s -> m (ZipList s)
templateS cnv _get (Var v) = return $ cnv v
templateS _cnv get (Get g) = get g
templateS _cnv _get (Str s) = return $ pure s
templateS cnv get (If (vl1, vl2) th el) = do
	val1 <- templateSs cnv get vl1
	val2 <- templateSs cnv get vl2
	ts <- templateSs cnv get th
	es <- templateSs cnv get el
	return $ (\v1 v2 t e -> if v1 == v2 then t else e)
		<$> val1 <*> val2 <*> ts <*> es
templateS cnv get (List ss) = pure . mconcat . getZipList
	<$> ((mconcat <$>) . sequenceA) <$> mapM (templateS cnv get) ss
templateS cnv get (OList ss) = ZipList . (: []) . mconcat . getZipList
	<$> ((mconcat <$>) . sequenceA) <$> mapM (templateS cnv get) ss
templateS _cnv _get (Plain p) = return $ pure p
templateS _cnv _get (OPlain p) = return $ ZipList [p]

templateSs :: (Applicative m, Monad m, Monoid s, Eq s) =>
	(s -> ZipList s) -> (s -> m (ZipList s)) -> [Syntax s] -> m (ZipList s)
templateSs cnv get ss = ((mconcat <$>) . sequenceA) <$> mapM (templateS cnv get) ss

data Syntax s
	= Var s | Get s | Str s
	| If ([Syntax s], [Syntax s]) [Syntax s] [Syntax s]
	| List [Syntax s] | OList [Syntax s]
	| Plain s | OPlain s
	deriving Show

isInf :: Syntax s -> Bool
isInf (List _) = True
isInf (Plain _) = True
isInf (If (v1, v2) t e) = all (all isInf) [v1, v2, t, e]
isInf _ = False

toOnce :: Syntax s -> Syntax s
toOnce (List ss) = OList ss
toOnce (Plain p) = OPlain p
toOnce s = s

checkInf :: Syntax s -> Syntax s
checkInf (List ss) | all isInf ss = List . map toOnce $ map checkInf ss
checkInf (If (v1, v2) t e) | all isInf (concat [v1, v2, t, e]) =
	If (map toOnce v1, v2) t e
checkInf s = s

syntax :: (StringLike s, IsString s) => s -> Maybe (Syntax s)
syntax s = case parses . processIf $ tokens s of
	(ss, []) -> Just $ List ss
	_ -> Nothing

parse :: [Token s] -> Maybe (Syntax s, [Token s])
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
parse (TOpen : ts) = case parses ts of
	(ss, TClose : ts') -> Just (List ss, ts')
	_ -> Nothing
parse (TPlain p : ts) = Just (Plain p , ts)
parse _ = Nothing

parses :: [Token s] -> ([Syntax s], [Token s])
parses ts = case parse ts of
	Just (s, ts') -> (s :) `first` parses ts'
	_ -> ([], ts)

processIf :: StringLike s => [Token s] -> [Token s]
processIf (TIf : ts) = TIf : uncurry (++) ((filter (not . isEmpty) `first`) $ span (/= TThen) ts)
processIf (t : ts) = t : processIf ts
processIf _ = []

data Token s
	= TVar s | TGet s | TStr s
	| TIf | TEq | TThen | TElse | TEnd
	| TOpen | TClose
	| TPlain s
	deriving (Show, Eq)

isEmpty :: StringLike s => Token s -> Bool
isEmpty (TPlain p) = sall isSpace p
isEmpty _ = False

class (Eq s, Monoid s) =>  StringLike s where
	sall :: (Char -> Bool) -> s -> Bool
	snull :: s -> Bool
	sempty :: s
	ssplitAt :: Int -> s -> (s, s)
	sspan :: (Char -> Bool) -> s -> (s, s)
	scons :: Char -> s -> s
	suncons :: s -> Maybe (Char, s)
	sunsnoc :: s -> Maybe (s, Char)

instance StringLike BSC.ByteString where
	sall = BSC.all
	snull = BSC.null
	sempty = BSC.empty
	ssplitAt = BSC.splitAt
	sspan = BSC.span
	scons = BSC.cons
	suncons = BSC.uncons
	sunsnoc = BSC.unsnoc

instance StringLike LBSC.ByteString where
	sall = LBSC.all
	snull = LBSC.null
	sempty = LBSC.empty
	ssplitAt = LBSC.splitAt . fromIntegral
	sspan = LBSC.span
	scons = LBSC.cons
	suncons = LBSC.uncons
	sunsnoc = LBSC.unsnoc

instance StringLike T.Text where
	sall = T.all
	snull = T.null
	sempty = T.empty
	ssplitAt = T.splitAt
	sspan = T.span
	scons = T.cons
	suncons = T.uncons
	sunsnoc t = if T.null t then Nothing else Just (T.init t, T.last t)

instance StringLike LT.Text where
	sall = LT.all
	snull = LT.null
	sempty = LT.empty
	ssplitAt = LT.splitAt . fromIntegral
	sspan = LT.span
	scons = LT.cons
	suncons = LT.uncons
	sunsnoc t = if LT.null t then Nothing else Just (LT.init t, LT.last t)

class StringLikeList c where
	toChar :: c -> Char
	fromChar :: Char -> c

instance (Eq c, StringLikeList c) => StringLike [c] where
	sall = all . (. toChar)
	snull = null
	sempty = []
	ssplitAt = splitAt
	sspan = span . (. toChar)
	scons = (:) . fromChar
	suncons = ((toChar `first`) <$>) . uncons'
	sunsnoc [] = Nothing
	sunsnoc cs = Just (init cs, toChar $ last cs)

sdropWhile :: StringLike s => (Char -> Bool) -> s -> s
sdropWhile = (snd .) . sspan

uncons' :: [a] -> Maybe (a, [a])
uncons' (x : xs) = Just (x, xs)
uncons' _ = Nothing

instance StringLikeList Char where toChar = id; fromChar = id

tokens :: (StringLike s, IsString s) => s -> [Token s]
tokens s | snull s = []
tokens s
	| ("\\if", r) <- ssplitAt 3 s = TIf : tokens (sdropWhile isSpace r)
	| ("\\==", r) <- ssplitAt 3 s = TEq : tokens (sdropWhile isSpace r)
	| ("\\then\n", r) <- ssplitAt 6 s = TThen : tokens r
	| ("\\else\n", r) <- ssplitAt 6 s = TElse : tokens r
	| ("\\end\n", r) <- ssplitAt 5 s = TEnd : tokens r
	| ("\\[\n", r) <- ssplitAt 3 s = TOpen : tokens r
	| ("\\]\n", r) <- ssplitAt 3 s = TClose : tokens r
	| ("\\", r) <- ssplitAt 1 s =
		let (c, r') = ssplitAt 1 r in TPlain c : tokens r'
	| ("${\"", r) <- ssplitAt 3 s = uncurry (:)
		. (TStr . fst . fromJust . sunsnoc *** tokens . snd . fromJust . suncons)
		$ sspan (/= '}') r
	| ("${", r) <- ssplitAt 2 s = uncurry (:)
		. (TVar *** tokens . snd . fromJust . suncons) $ sspan (/= '}') r
	| ("@{", r) <- ssplitAt 2 s = uncurry (:)
		. (TGet *** tokens . snd . fromJust . suncons) $ sspan (/= '}') r
tokens s = case sspan (`notElem` "$@\\") s of
	(p, s') -> TPlain p : tokens s'
