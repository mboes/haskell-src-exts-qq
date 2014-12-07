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
-- > let x = Hs.name "n" in [hs| \ ((x)) -> $(Hs.Var (Hs.UnQual x)) + 1 |]
--
-- Alternatively, one can use the double underscore syntax, useful when
-- antiquoting a function name as in the following:
--
-- > let f = "incr"
-- >     fE = Hs.Var $ Hs.UnQual $ Hs.name f
-- > in [hs| let __f__ x = x + 1 in $fE 10 |]
--
-- The double parentheses syntax is also used for antiquoting types. For
-- instance:
--
-- > let typ = Hs.TyCon (Hs.UnQual $ Hs.name "Int")
-- > in [hs| 1 :: ((typ)) |]
--
-- In a pattern context, antiquotations use the same syntax.

module Language.Haskell.Exts.QQ (hs, dec, decs, ty,
    hsWithMode, decWithMode, decsWithMode, tyWithMode) where

import qualified Language.Haskell.Exts as Hs
import qualified Language.Haskell.Meta.Syntax.Translate as Hs
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib
import Data.Generics
import Data.List (intercalate, isPrefixOf, isSuffixOf)

allExtensions :: Hs.ParseMode
allExtensions = Hs.defaultParseMode{Hs.extensions = known}
  where
#if MIN_VERSION_haskell_src_exts(1,14,0)
   known = [ext | ext@Hs.EnableExtension{} <- Hs.knownExtensions]
#else
   known = Hs.knownExtensions
#endif


-- | A quasiquoter for expressions. All Haskell extensions known by
-- haskell-src-exts are activated by default.
hs :: QuasiQuoter
hs = hsWithMode allExtensions

-- | A quasiquoter for types. All Haskell extensions known by
-- haskell-src-exts are activated by default.
ty :: QuasiQuoter
ty = tyWithMode allExtensions

-- | A quasiquoter for a single top-level declaration.
dec :: QuasiQuoter
dec = decWithMode allExtensions

-- | A quasiquoter for multiple top-level declarations.
decs :: QuasiQuoter
decs = decsWithMode allExtensions

-- | Rather than importing the above quasiquoters, one can create custom
-- quasiquoters with a customized 'ParseMode' using this function.
--
-- > hs = hsWithMode mode
-- > dec = decWithMode mode
-- > decs = decsWithMode mode
hsWithMode :: Hs.ParseMode -> QuasiQuoter
hsWithMode = qq . Hs.parseExpWithMode

decWithMode :: Hs.ParseMode -> QuasiQuoter
decWithMode = qq . Hs.parseDeclWithMode

decsWithMode :: Hs.ParseMode -> QuasiQuoter
decsWithMode mode = qq $ \src -> fmap strip $ Hs.parseModuleWithMode mode src
    where
        -- Implementation note, to parse multiple decls it's (ab)used that a
        -- listing of decls (possibly with import istatements and other extras)
        -- is a valid module.
        strip :: Hs.Module -> [Hs.Decl]
        strip (Hs.Module _ _ _ _ _ _ decs) = decs

tyWithMode :: Hs.ParseMode -> QuasiQuoter
tyWithMode = qq . Hs.parseTypeWithMode

qq :: Data a => (String -> Hs.ParseResult a) -> QuasiQuoter
qq parser = QuasiQuoter { quoteExp = parser `project` antiquoteExp
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
-- names for syntax constructors previous to GHC 7.4.1. This has the unfortunate
-- effect of breaking quotation when the haskell-src-exts syntax module is
-- imported qualified. The solution is to set the flavour of all names to
-- 'NameG' on older versions of GHC.
-- See also <http://www.haskell.org/pipermail/glasgow-haskell-users/2013-February/023793.html>.
qualify :: Name -> Name
#if defined(__GLASGOW_HASKELL__) &&  __GLASGOW_HASKELL__ < 704
-- Need special cases for constructors used in string literals. Assume nearly
-- all else is a datatype defined in Syntax module of haskell-src-exts.
qualify n | ":" <- nameBase n = '(:)
          | "[]" <- nameBase n = '[]
          | "(,)" <- nameBase n = '(,)
          | "Nothing" <- nameBase n = 'Nothing
          | "Just" <- nameBase n = 'Just
          | "True"      <- nameBase n = 'True
          | "False"     <- nameBase n = 'False
          | "Left"      <- nameBase n = 'Left
          | "Right"     <- nameBase n = 'Right
          | "LT"        <- nameBase n = 'LT
          | "EQ"        <- nameBase n = 'EQ
          -- GT is also exported by Data.Generics
          | "GT"        <- nameBase n = 'Prelude.GT
          | "SrcLoc" <- nameBase n = 'Hs.SrcLoc
          | "Boxed" <- nameBase n = 'Hs.Boxed
          | otherwise = Name (mkOccName (nameBase n)) flavour
    where pkg = "haskell-src-exts-" ++ VERSION_haskell_src_exts
          flavour = NameG VarName (mkPkgName pkg)
                    (mkModName "Language.Haskell.Exts.Syntax")
#else
qualify n = n
#endif

antiquoteExp :: Data a => a -> Q Exp
antiquoteExp t = dataToQa (conE . qualify) litE (foldl appE)
                 (const Nothing `extQ` antiE `extQ` antiP `extQ` antiN `extQ` antiT) t
    where antiE (Hs.SpliceExp (Hs.IdSplice v)) = Just $ varE $ mkName v
          antiE (Hs.SpliceExp (Hs.ParenSplice e)) = Just $ return $ Hs.toExp e
          antiE _ = Nothing
          antiP (Hs.PParen (Hs.PParen (Hs.PVar (Hs.Ident n)))) =
              Just $ appE [| Hs.PVar |] (varE (mkName n))
          antiP _ = Nothing
          antiT (Hs.TyParen (Hs.TyParen (Hs.TyVar (Hs.Ident n)))) = Just . varE $ mkName n
          antiT _ = Nothing
          antiN (Hs.Ident n) | "__" `isPrefixOf` n, "__" `isSuffixOf` n  =
            let nn = take (length n - 4) (drop 2 n)
            in Just $ appE [| Hs.Ident |] (varE (mkName nn))
          antiN _ = Nothing

antiquotePat :: Data a => a -> Q Pat
antiquotePat = dataToQa qualify litP conP (const Nothing `extQ` antiP)
    where antiP (Hs.PParen (Hs.PParen (Hs.PVar (Hs.Ident n)))) =
              Just $ conP 'Hs.PVar [varP (mkName n)]
          antiP _ = Nothing
