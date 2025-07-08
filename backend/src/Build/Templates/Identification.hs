module Build.Templates.Identification where
import qualified Data.Map.Strict
import           Language.Haskell.TH.Lib    (DecsQ, ExpQ)
import           Language.Haskell.TH.Syntax (Body (NormalB), Dec (SigD, ValD),
                                             Exp (AppE, ConE, ListE, LitE, TupE, UnboundVarE, VarE),
                                             Lit (IntegerL), Name, Pat (VarP),
                                             Q, Type (AppT, ConT), mkName,
                                             nameBase)
import           Model.GameState            (ActionF, ResolutionF)
import           Model.GID                  (GID (GID))
import           Model.Mappings             (GIDToDataMap (GIDToDataMap))
import           Model.Parser.Lexer         (Lexeme)
import           Prelude                    hiding (exp)

labelTemplate :: Name -> String -> Lexeme -> DecsQ
labelTemplate typeName binding lexeme = pure desc
  where
    desc        = [sigd,vald]
    sigd        = SigD decLabel appt
    appt        = AppT labelType' objectType'
    decLabel    = mkName (binding <> "Label")
    labelType'  = ConT labelName
    objectType' = ConT typeName  -- Use the Name directly
    vald = ValD (VarP decLabel) eval' []
    eval' = NormalB (AppE (ConE labelName) uvar)
    uvar = UnboundVarE $ (mkName . show) lexeme
    labelName = mkName "Model.Label.Label"

makeLabels :: Name -> [(String, Lexeme)] -> DecsQ
makeLabels typeName bindingLexemePairs = do
  declarations <- mapM (uncurry (labelTemplate typeName)) bindingLexemePairs
  pure (concat declarations)

gidDeclaration :: String -> String -> Integer -> DecsQ
gidDeclaration tag' binding' literal = pure [sigd, value]
  where
    sigd    = SigD binding type'
    binding = mkName (binding' <> "GID")
    type'   = AppT (ConT gid) (ConT tag)
    gid     = mkName "GID"
    tag     = mkName tag'
    value = ValD (VarP binding)
                 (NormalB
                   (AppE
                     (ConE gid)
                     (LitE (IntegerL literal)))) []

makeActionGIDsAndMap :: [ExpQ] -> Q [Dec]
makeActionGIDsAndMap expQs = do
  exps <- sequence expQs
  let numberedPairs = zip exps [1..]

  -- Generate GID declarations (same as makeActionGIDsAuto)
  gidDeclarations <- concat <$> mapM (uncurry makeActionGID) numberedPairs

  -- Generate the map
  mapDeclaration <- makeMapDeclaration numberedPairs

  pure (gidDeclarations ++ [mapDeclaration])

-- Helper function to create a single GID declaration (from Build.Identifiers)
makeActionGID :: Exp -> Int -> Q [Dec]
makeActionGID exp gidValue = do
  case exp of
    VarE functionName -> do
      let originalNameStr = nameBase functionName
          gidNameStr = originalNameStr ++ "GID"
          gidName = mkName gidNameStr
          gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))
          gidType = AppT (ConT ''GID) (AppT (ConT ''ActionF) (ConT ''ResolutionF))

      pure [ SigD gidName gidType
           , ValD (VarP gidName) (NormalB gidExpr) []
           ]
    _ -> fail "makeActionGID expects a simple variable name"

-- Helper function to create the map declaration
makeMapDeclaration :: [(Exp, Int)] -> Q Dec
makeMapDeclaration numberedPairs = do
  let mapName = mkName "actionMap"
      mapType = AppT (ConT ''GIDToDataMap)
                     (AppT (AppT (ConT ''ActionF) (ConT ''ResolutionF))
                           (AppT (ConT ''ActionF) (ConT ''ResolutionF)))

      -- Create list of (GID, Action) tuples
      tupleExps = map makeTuple numberedPairs
      listExp = ListE tupleExps

      -- GIDToDataMap (Map.fromList [...])
      mapFromListExp = AppE (VarE 'Data.Map.Strict.fromList) listExp
      gidToDataMapExp = AppE (ConE 'GIDToDataMap) mapFromListExp

  pure $ ValD (VarP mapName) (NormalB gidToDataMapExp) []

-- Helper to create (gid', action) tuple expressions
makeTuple :: (Exp, Int) -> Exp
makeTuple (exp, _gidValue) =
  case exp of
    VarE functionName ->
      let originalNameStr = nameBase functionName
          gidNameStr = originalNameStr ++ "GID"
          gidName = mkName gidNameStr
      in TupE [Just (VarE gidName), Just (VarE functionName)]
    _ -> error "Expected VarE in makeTuple"
