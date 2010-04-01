module Language.Haskell.Exts.QQ (hs, dec) where

import qualified Language.Haskell.Exts.Syntax as Hs
import qualified Language.Haskell.Exts.Parser as Hs
import qualified Language.Haskell.Exts.Extension as Hs
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
qualify n | ":" <- nameBase n = n
          | "[]" <- nameBase n = n
          | "Nothing" <- nameBase n = n
          | "Just" <- nameBase n = n
          | "SrcLoc" <- nameBase n = Name (mkOccName (nameBase n)) (flav "SrcLoc")
          | otherwise = Name (mkOccName (nameBase n)) (flav "Syntax")
    where pkg = "haskell-src-exts-" ++ VERSION_haskell_src_exts
          flav mod = NameG VarName (mkPkgName pkg)
                     (mkModName ("Language.Haskell.Exts." ++ mod))

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
antiquotePat = dataToQa qualify litP conP (const Nothing)
