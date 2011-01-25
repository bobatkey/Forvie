module Text.Lexeme
    ( Lexeme(..) )
    where

import qualified Data.Text as T
import           Text.Position (Span, Regioned (..))

data Lexeme tok = Lexeme { lexemeTok  :: tok
                         , lexemePos  :: Span
                         , lexemeText :: T.Text
                         }
                deriving (Eq, Ord, Show)

instance Regioned (Lexeme tok) where
    regionLeft  = regionLeft . lexemePos
    regionRight = regionRight . lexemePos
