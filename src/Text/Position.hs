module Text.Position
    ( Position (..)
    , initPos
    , updatePos
    , Span (..)
    , Regioned (..)
    , makeSpan
    )
    where

data Position = Position { posCharNum   :: !Int
                         , posLineNum   :: !Int
                         , posColumnNum :: !Int
                         }
              deriving (Eq, Ord)

instance Show Position where
    show (Position char line col) =
        "(Position " ++ show char ++ " " ++ show line ++ " " ++ show col ++ ")"

initPos :: Position
initPos = Position 0 1 0

updatePos :: Position -> Char -> Position
updatePos (Position cnum lnum colnum) '\n' = Position (cnum + 1) (lnum + 1) 0
updatePos (Position cnum lnum colnum) _    = Position (cnum + 1) lnum (colnum + 1)

data Span = Span Position Position
            deriving (Eq, Ord, Show)

class Regioned r where
    regionLeft  :: r -> Position
    regionRight :: r -> Position

makeSpan :: (Regioned r, Regioned r') => r -> r' -> Span
makeSpan x y = Span (regionLeft x) (regionRight y)

instance Regioned Span where
  regionLeft (Span l _)  = l
  regionRight (Span _ r) = r
