import Data.List

infixl 8 :>:
infixl 9 :|:

data Symbol = Character Char
            | Wildcard
            | Empty
  deriving (Eq)

instance Show Symbol where
  show (Character c) = [c]
  show Wildcard = "*"
  show Empty = "0"

data RegEx = Sym Symbol
            | RegEx :>: RegEx
            | RegEx :|: RegEx
            | Star RegEx
            | Opt RegEx
  deriving (Eq)

instance Show RegEx where
  show (Sym s) = show s
  show (r :>: r') = "(" ++ show r ++ show r' ++ ")"
  show (r :|: r') = "(" ++ show r ++ "|" ++ show r' ++ ")"
  show (Star r) = show r ++ "*"
  show (Opt r) = show r ++ "?"

char :: Char -> RegEx
char = Sym . Character

wildcard :: RegEx
wildcard = Sym Wildcard

empty :: RegEx
empty = Sym Empty

rep :: RegEx -> Int -> RegEx
rep r 1 = r
rep r n = r :>: rep r (n - 1)

data ContextRegEx = Top
                  | SeqLeft ContextRegEx RegEx
                  | SeqRight RegEx ContextRegEx
                  | OrLeft ContextRegEx RegEx
                  | OrRight RegEx ContextRegEx
                  | InStar ContextRegEx
                  | InOpt ContextRegEx
  deriving (Show, Eq)

data Position = Before | After
  deriving (Show, Eq)

data ZipperRegEx = ZipperRegEx Position RegEx ContextRegEx
  deriving (Show, Eq)

start :: RegEx -> ZipperRegEx
start r = ZipperRegEx Before r Top

data ParseState a = ParseState { state :: ZipperRegEx, info :: a }
  deriving Eq

--data Transition a = Transition { label :: a -> Maybe a, target :: ZipperRegEx }
data Transition = Transition { label :: Symbol, target :: ZipperRegEx }
  deriving (Show, Eq)

readSym :: Symbol -> String -> Maybe String
readSym (Character c) []     = Nothing
readSym (Character c) (ch:s)
  | c == ch                  = Just s
  | otherwise                = Nothing
readSym Wildcard      []     = Nothing
readSym Wildcard      (_:s)  = Just s
readSym Empty         s      = Just s

readEmpty :: String -> Maybe String
readEmpty = Just

isTransitionOver :: Symbol -> Transition -> Bool
isTransitionOver (Character c) z = Character c == label z || Wildcard == label z
isTransitionOver Empty         z = Empty == label z
isTransitionOver Wildcard      z = Empty /= label z





transition :: ZipperRegEx -> [Transition {-String-}]
transition (ZipperRegEx Before (Sym c)    con)              = [Transition c{-(readSym c)-}       (ZipperRegEx After  (Sym c)    con)]
transition (ZipperRegEx Before (r :>: r') con)              = [Transition Empty {-readEmpty  -}  (ZipperRegEx Before r          (SeqLeft con r'))]
transition (ZipperRegEx Before (r :|: r') con)              = [Transition Empty {-readEmpty  -}  (ZipperRegEx Before r          (OrLeft con r'))
                                                              ,Transition Empty {-readEmpty  -}  (ZipperRegEx Before r'         (OrRight r con))
                                                              ]
transition (ZipperRegEx Before (Star r)   con)              = [Transition Empty {-readEmpty-}    (ZipperRegEx Before r          (InStar con))
                                                              ,Transition Empty {-readEmpty-}    (ZipperRegEx After  (Star r)   con)
                                                              ]
transition (ZipperRegEx Before (Opt r)    con)              = [Transition Empty (ZipperRegEx Before r (InOpt con))
                                                              ,Transition Empty (ZipperRegEx After (Opt r) con)
                                                              ]
transition (ZipperRegEx After  r          Top)              = []
transition (ZipperRegEx After  r          (SeqLeft con r')) = [Transition Empty{-readEmpty-}    (ZipperRegEx Before r'         (SeqRight r con))]
transition (ZipperRegEx After  r'         (SeqRight r con)) = [Transition Empty{-readEmpty-}    (ZipperRegEx After  (r :>: r') con)]
transition (ZipperRegEx After  r          (OrLeft con r'))  = [Transition Empty{-readEmpty-}    (ZipperRegEx After  (r :|: r') con)]
transition (ZipperRegEx After  r'         (OrRight r con))  = [Transition Empty{-readEmpty-}    (ZipperRegEx After  (r :|: r') con)]
transition (ZipperRegEx After  r          (InStar con))     = [Transition Empty{-readEmpty-}    (ZipperRegEx Before r          (InStar con))
                                                              ,Transition Empty{-readEmpty-}    (ZipperRegEx After  (Star r)   con)
                                                              ]
transition (ZipperRegEx After  r          (InOpt con))      = [Transition Empty                 (ZipperRegEx After  (Opt r)    con)]




haltsTrivially :: ZipperRegEx -> Bool
haltsTrivially (ZipperRegEx Before (Sym c) con) = False
haltsTrivially (ZipperRegEx Before (r :>: r') con) = haltsTrivially (ZipperRegEx Before r (SeqLeft con r'))
haltsTrivially (ZipperRegEx Before (r :|: r') con) = haltsTrivially (ZipperRegEx Before r (OrLeft con r'))
                                                  || haltsTrivially (ZipperRegEx Before r' (OrRight r con))
haltsTrivially (ZipperRegEx Before (Star r) con) = haltsTrivially (ZipperRegEx After (Star r) con)
haltsTrivially (ZipperRegEx Before (Opt r) con) = haltsTrivially (ZipperRegEx After (Opt r) con)
haltsTrivially (ZipperRegEx After r Top) = True
haltsTrivially (ZipperRegEx After r (SeqLeft con r')) = haltsTrivially (ZipperRegEx Before r' (SeqRight r con))
haltsTrivially (ZipperRegEx After r' (SeqRight r con)) = haltsTrivially (ZipperRegEx After (r :>: r) con)
haltsTrivially (ZipperRegEx After r (OrLeft con r')) = haltsTrivially (ZipperRegEx After (r :|: r') con)
haltsTrivially (ZipperRegEx After r' (OrRight r con)) = haltsTrivially (ZipperRegEx After (r :|: r') con)
haltsTrivially (ZipperRegEx After r (InStar con)) = haltsTrivially (ZipperRegEx After (Star r) con)
haltsTrivially (ZipperRegEx After r (InOpt con)) = haltsTrivially (ZipperRegEx After (Opt r) con)





transitionOverSymbol :: Symbol -> ZipperRegEx -> [ZipperRegEx]
transitionOverSymbol s z = go [] [z] []
  where go :: [ZipperRegEx] -> [ZipperRegEx] -> [ZipperRegEx] -> [ZipperRegEx]
        go seen []        successful = successful
        go seen unchecked successful = let transitions = concatMap transition (unchecked \\ seen)
                                           newSuccessful = successful ++ map target (filter (isTransitionOver s) transitions)
                                           nextByEmpty = map target $ filter (isTransitionOver Empty) transitions
                                           newSeen = seen ++ unchecked
                                       in go newSeen nextByEmpty newSuccessful

go :: [ZipperRegEx] -> String -> Bool
go qs []    = any haltsTrivially qs
go qs (c:s) = let newqs = nub $ concatMap (transitionOverSymbol (Character c)) qs
              in go newqs s

match :: RegEx -> String -> Bool
match r s = go [start r] s

rexp :: RegEx
rexp = rep (Opt (char 'a')) 32 :>: rep (char 'a') 32

main = putStr $ show $ match rexp "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
