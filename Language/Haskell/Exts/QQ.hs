-- | This module defines quasiquoters for haskell-src-exts expressions and
-- declarations.
--
-- Antiquotations steal the splice syntax of Template Haskell, so for
-- example example 'x' appears antiquoted in @[$hs| $x ++ $(Hs.strE \"bar\") |]@.
-- Expressions appearing inside parenthesized splices are limited to concrete
-- syntax expressible by Template Haskell's 'Exp' data type.
--
-- Names in patterns can also be antiquoted, using double parentheses. For
-- instance:
--
-- > let f = Hs.name "foo" in [$dec| ((f)) x = x + x |]
--
-- In a pattern context, antiquotations use the same syntax.

module Language.Haskell.Exts.QQ (hs, dec) where

import qualified Language.Haskell.Exts as Hs
import qualified Language.Haskell.Exts.Translate as Hs
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib
import Data.Generics
import Data.List (intercalate)


hs = QuasiQuoter { quoteExp = Hs.parseExpWithMode
                              Hs.defaultParseMode{Hs.extensions = Hs.knownExtensions}
                                    `project` antiquoteExp
                 , quotePat = Hs.parsePat `project` antiquotePat
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 613
                 , quoteType = error "Unimplemented."
                 , quoteDec = error "Unimplemented."
#endif
                 }

dec = QuasiQuoter { quoteExp = Hs.parseDeclWithMode
                               Hs.defaultParseMode{Hs.extensions = Hs.knownExtensions}
                                     `project` antiquoteExp
                  , quotePat = Hs.parsePat `project` antiquotePat
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 613
                  , quoteType = error "Unimplemented."
                  , quoteDec = error "Unimplemented."
#endif
                  }

project f k s = case f s of
                  Hs.ParseOk x -> k x
                  Hs.ParseFailed loc err -> fail err

-- | The generic functions in 'Language.Haskell.TH.Quote' don't use global
-- names for syntax constructors. This has the unfortunate effect of breaking
-- quotation when the haskell-src-exts syntax module is imported qualified.
-- The solution is to set the flavour of all names to 'NameG'.
qualify :: Name -> Name
-- Need special cases for constructors used in string literals. Assume nearly
-- all else is a datatype defined in Syntax module of haskell-src-exts.
qualify n | ":" <- nameBase n = '(:)
          | "[]" <- nameBase n = '[]
          | "Nothing" <- nameBase n = 'Nothing
          | "Just" <- nameBase n = 'Just
          | "SrcLoc" <- nameBase n = 'Hs.SrcLoc
          | otherwise = Name (mkOccName (nameBase n)) flavour
    where pkg = "haskell-src-exts-" ++ VERSION_haskell_src_exts
          flavour = NameG VarName (mkPkgName pkg)
                    (mkModName "Language.Haskell.Exts.Syntax")

antiquoteExp :: Data a => a -> Q Exp
antiquoteExp t = dataToQa (conE . qualify) litE (foldl appE)
                 (const Nothing `extQ` antiE `extQ` antiP) t
    where antiE (Hs.SpliceExp (Hs.IdSplice v)) = Just $ varE $ mkName v
          antiE (Hs.SpliceExp (Hs.ParenSplice e)) = Just $ return $ Hs.toExp e
          antiE _ = Nothing
          antiP (Hs.PParen (Hs.PParen (Hs.PVar (Hs.Ident n)))) =
              Just $ appE [| Hs.PVar |] (varE (mkName n))
          antiP _ = Nothing

antiquotePat :: Data a => a -> Q Pat
antiquotePat = dataToQa qualify litP conP (const Nothing `extQ` antiP)
    where antiE (Hs.SpliceExp (Hs.IdSplice v)) = Just $ varP $ mkName v
          antiE (Hs.SpliceExp (Hs.ParenSplice e)) =
            case Hs.parsePat $ Hs.prettyPrint e of
              Hs.ParseOk p -> Just $ return $ Hs.toPat p
              Hs.ParseFailed _ err -> Just $ fail err
          antiE _ = Nothing
          antiP (Hs.PParen (Hs.PParen (Hs.PVar (Hs.Ident n)))) =
              Just $ conP 'Hs.PVar [varP (mkName n)]
          antiP _ = Nothing

