{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PlutusCore.Lexer.Type
    ( Keyword (..)
    , Special (..)
    , Token (..)
    , allKeywords
    , IdentifierState
    , newIdentifier
    , emptyIdentifierState
    , identifierStateFrom
    ) where

import           PlutusPrelude

import           Language.PlutusCore.Name

import           Control.Monad.State
import qualified Data.Map                 as M
import qualified Data.Text                as T

-- | A keyword in Plutus Core.
data Keyword
    = KwAbs
    | KwLam
    | KwIFix
    | KwFun
    | KwAll
    | KwType
    | KwProgram
    | KwCon
    | KwIWrap
    | KwBuiltin
    | KwUnwrap
    | KwError
    deriving (Show, Eq, Enum, Bounded, Generic, NFData)

-- | A special character. This type is only used internally between the lexer
-- and the parser.
data Special
    = OpenParen
    | CloseParen
    | OpenBracket
    | CloseBracket
    | Dot
    | OpenBrace
    | CloseBrace
    deriving (Show, Eq, Generic, NFData)

-- | A token generated by the tker.
data Token ann
    = TkName  { tkLoc        :: ann
              , tkName       :: T.Text  -- String??
              , tkIdentifier :: Unique -- ^ A 'Unique' assigned to the identifier during lexing.
              }
    | TkBuiltinFnId    { tkLoc :: ann, tkBuiltinFnId   :: T.Text }
    | TkBuiltinTypeId  { tkLoc :: ann, tkBuiltinTypeId :: T.Text }
    | TkLiteralConst   { tkLoc :: ann, tkLiteralConst  :: T.Text }
    | TkNat            { tkLoc :: ann, tkNat           :: Natural }
    | TkKeyword        { tkLoc :: ann, tkKeyword       :: Keyword }
    | TkSpecial        { tkLoc :: ann, tkSpecial       :: Special }
    | EOF              { tkLoc :: ann }
    deriving (Show, Eq, Generic, NFData, Functor)

instance Pretty Special where
    pretty OpenParen    = "("
    pretty CloseParen   = ")"
    pretty OpenBracket  = "["
    pretty CloseBracket = "]"
    pretty Dot          = "."
    pretty OpenBrace    = "{"
    pretty CloseBrace   = "}"

instance Pretty Keyword where
    pretty KwAbs     = "abs"
    pretty KwLam     = "lam"
    pretty KwIFix    = "ifix"
    pretty KwFun     = "fun"
    pretty KwAll     = "all"
    pretty KwType    = "type"
    pretty KwProgram = "program"
    pretty KwCon     = "con"
    pretty KwIWrap   = "iwrap"
    pretty KwBuiltin = "builtin"
    pretty KwUnwrap  = "unwrap"
    pretty KwError   = "error"

instance Pretty (Token ann) where
    pretty (TkName _ n _)            = pretty n
    pretty (TkNat _ n)               = pretty n
    pretty (TkBuiltinFnId _ ident)   = pretty ident
    pretty (TkBuiltinTypeId _ ident) = pretty ident
    pretty (TkLiteralConst _ lit)    = pretty lit
    pretty (TkKeyword _ kw)          = pretty kw
    pretty (TkSpecial _ s)           = pretty s
    pretty EOF{}                     = mempty

-- | The list of all 'Keyword's.
allKeywords :: [Keyword]
allKeywords = [minBound .. maxBound]

-- | An 'IdentifierState' includes a map indexed by 'Int's as well as a map
-- indexed by 'ByteString's. It is used during parsing.
type IdentifierState = (M.Map T.Text Unique, Unique)

emptyIdentifierState :: IdentifierState
emptyIdentifierState = (mempty, Unique 0)

identifierStateFrom :: Unique -> IdentifierState
identifierStateFrom u = (mempty, u)

newIdentifier :: (MonadState IdentifierState m) => T.Text -> m Unique
newIdentifier str = do
    (ss, nextU) <- get
    case M.lookup str ss of
        Just k -> pure k
        Nothing -> do
            let nextU' = Unique $ unUnique nextU + 1
            put (M.insert str nextU ss, nextU')
            pure nextU
