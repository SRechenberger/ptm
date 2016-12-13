{-# LANGUAGE TemplateHaskell #-}
module Text.PTM.Lang
  (
  -- * Lenses
    title, toc, titlePage, width, height, alignV, alignH
  )
where

import Control.Lens hiding (noneOf)

import Text.Megaparsec
import Text.Megaparsec.Text
-- import Text.Megaparsec.Char
-- import Text.Megaparsec.Combinator
import qualified Text.Megaparsec.Lexer as L

import Control.Monad (void)
import Control.Applicative (empty)

import Data.List (intercalate)

--------------------------------------------------------------------------------
-- PTM -------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Syntax (indentation sensitive):
--
--    DOCUMENT ::=
--      'document'
--        OPTION*
--      'begin'
--        DOC_BODY
--      'end'
--    DOC_BODY ::=
--      'section'
--        OPTION*
--      'begin'
--        DOC_BODY
--      'end'
--    | 'text'
--        OPTION*
--      'begin'
--        TEXT
--      'end'
--    OPTION ::= VAR := VALUE
--    VAR ::= [a..z]*
--    VALUE ::= '"' .* '"' | [0..9]* | true | false
--    TEXT ::= WORD*
--    WORD ::= [^\t\n\ ]*
--
--------------------------------------------------------------------------------

data Alignment = Begin | Mid | End deriving (Show, Eq)

data Options = Options
  { _title :: Maybe String
  , _toc :: Bool
  , _titlePage :: Bool
  , _width :: Maybe Int
  , _height :: Maybe Int
  , _alignV :: Alignment
  , _alignH :: Alignment
  } deriving Show

makeLenses ''Options

type Body = Either Text Section

data Option = String := String
  deriving Show

data Document = Document [Option] [Either Text Section]
  deriving Show

data Text = Text [Option] String
  deriving Show

data Section = Section [Option] [Either Text Section]
  deriving Show

ignore, ignore' :: Parser ()
-- ignore = many (oneOf "\t \n") >> return ()
ignore = L.space (void spaceChar) empty empty

-- ignore' = many (oneOf "\t ") >> return ()
ignore' = L.space (void $ oneOf " \t") empty empty

tok :: String -> Parser String
tok s = L.symbol ignore s

tok' :: String -> Parser String
tok' s = L.symbol ignore' s

document :: Parser Document
document = undefined

aLine:: Parser String
aLine = unwords . words <$> (some $ noneOf "\n")

ident :: Parser String
ident = do
  i <- some $ oneOf ['a'..'z']
  ignore'
  return i

word :: Parser String
word = L.lexeme ignore' (some (noneOf " \t\n"))

str :: Parser String
str = char '"' >> manyTill anyChar (char '"')

text :: Parser Text
text = do
  os <- L.indentBlock ignore $ do
    tok' "text"
    return $ L.IndentMany Nothing return $ do
      i <- ident
      tok ":="
      v <- (many digitChar <* ignore' <|> tok' "true" <|> tok' "false" <|> str)
      return $ i := v


  ls <- L.indentBlock ignore $ do
    tok' "begin"
    return $ L.IndentMany Nothing (return . concat) (many word)

  L.indentBlock ignore $ do
    tok' "end"
    return $ L.IndentNone ()

  return $ Text os (unwords ls)


