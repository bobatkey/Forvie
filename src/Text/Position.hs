module Text.Position
    ( Position (..)
    , initPos
    , updatePos
    , Span (..)
    , Regioned (..)
    )
    where

data Position = Position { posCharNum   :: Int
                         , posLineNum   :: Int
                         , posColumnNum :: Int
                         }
              deriving (Eq, Ord, Show)

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
  
{-
instance Regioned r => Regioned (AnnotRec r f) where
  regionLeft (Annot r _) = regionLeft r
  regionRight (Annot r _) = regionRight r
-}

instance Regioned r => Regioned [r] where
  regionLeft (x:_) = regionLeft x
  regionRight l    = regionRight (last l)