module Build.Templates.Identification where
import qualified Data.Map.Strict
import           Language.Haskell.TH        (reify)
import           Language.Haskell.TH.Lib    (DecsQ, ExpQ)
import           Language.Haskell.TH.Syntax (Body (NormalB), Dec (SigD, ValD),
                                             Exp (AppE, ConE, ListE, LitE, TupE, UnboundVarE, VarE),
                                             Info (VarI), Lit (IntegerL), Name,
                                             Pat (VarP), Q,
                                             Type (AppT, ArrowT, ConT, ForallT),
                                             mkName, nameBase)
import           Model.GameState            (ActionF, ActionMap, ResolutionT)
import           Model.GID                  (GID (GID))
import           Model.Mappings             (GIDToDataMap (GIDToDataMap))
import           Model.Parser.Lexer         (Lexeme)
import           Prelude                    hiding (exp)

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

labelTemplate :: Exp -> Lexeme -> DecsQ
labelTemplate exp lexeme = do
  case exp of
    VarE varName -> do
      -- Reify to get type information
      info <- reify varName
      valueType <- case info of
        VarI _ typ _ -> extractType typ
        _            -> fail $ "Expected a variable, got: " ++ show info

      let originalNameStr = nameBase varName
          labelNameStr = originalNameStr ++ "'"
          labelName = mkName labelNameStr
          lexemeVar = UnboundVarE (mkName (show lexeme))
          labelConstructor = ConE (mkName "Model.Label.Label")
          labelExpr = AppE labelConstructor lexemeVar
          labelTypeConstructor = ConT (mkName "Model.Label.Label")
          labelType = AppT labelTypeConstructor valueType

      pure [ SigD labelName labelType
           , ValD (VarP labelName) (NormalB labelExpr) []
           ]
    _ -> fail "labelTemplate expects a simple variable name"

makeLabels :: [(ExpQ, Lexeme)] -> DecsQ
makeLabels expLexemePairs = do
  exps <- mapM fst expLexemePairs
  let lexemes = map snd expLexemePairs
  let pairs = zip exps lexemes

  declarations <- mapM (uncurry labelTemplate) pairs
  pure (concat declarations)
    {-
makeActionGIDsAndMap :: [ExpQ] -> Q [Dec]
makeActionGIDsAndMap expQs = do
  exps <- sequence expQs

  let numberedPairs :: [(Exp, Int)]
      numberedPairs = zip exps [1..]

  -- Generate GID declarations
  gidDeclarations <- concat <$> mapM (uncurry makeActionGID) numberedPairs

  -- Generate the map using makeMapDeclarationWithName directly (most efficient)
  mapDeclaration <- makeActionMapDeclaration numberedPairs

  pure (gidDeclarations ++ [mapDeclaration])
-}

makeActionGIDsAndMap :: [ExpQ] -> Q [Dec]
makeActionGIDsAndMap expQs = do
  exps <- sequence expQs

  let numberedPairs :: [(Exp, Int)]
      numberedPairs = zip exps [1..]

  -- Generate GID declarations
  gidDeclarations <- concat <$> mapM (uncurry makeActionGID) numberedPairs

  -- Generate the ActionMap declaration with explicit type signature
  mapDeclarations <- makeActionMapDeclaration numberedPairs

  pure (gidDeclarations ++ mapDeclarations)

makeActionMapDeclaration :: [(Exp, Int)] -> Q [Dec]
makeActionMapDeclaration [] = fail "Cannot create ActionMap from empty list"
makeActionMapDeclaration numberedPairs = do
  let mapName = mkName "actionMap"
      -- Explicit ActionMap type signature
      actionMapType = ConT ''ActionMap
      typeSignature = SigD mapName actionMapType

  -- Use existing function to create the value declaration
  valueDeclaration <- makeMapDeclarationWithName "actionMap" numberedPairs

  pure [typeSignature, valueDeclaration]

-- Helper function to create a single GID declaration (from Build.Identifiers)
makeActionGID :: Exp -> Int -> Q [Dec]
makeActionGID exp gidValue = do
  case exp of
    VarE functionName -> do
      let originalNameStr = nameBase functionName
          gidNameStr = originalNameStr ++ "GID"
          gidName = mkName gidNameStr
          gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))
          gidType = AppT (ConT ''GID) (AppT (ConT ''ActionF)
                      (AppT (ConT ''ResolutionT) (ConT ''())))

      pure [ SigD gidName gidType
           , ValD (VarP gidName) (NormalB gidExpr) []
           ]
    _ -> fail "makeActionGID expects a simple variable name"
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

makeGID :: Exp -> Int -> Q [Dec]
makeGID exp gidValue = do
  case exp of
    VarE name -> do
      -- Reify to get type information
      info <- reify name
      valueType <- case info of
        VarI _ typ _ -> extractType typ
        _            -> fail $ "Expected a variable, got: " ++ show info

      let originalNameStr = nameBase name
          gidNameStr = originalNameStr ++ "GID"
          gidName = mkName gidNameStr
          gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))
          gidType = AppT (ConT ''GID) valueType

      pure [ SigD gidName gidType
           , ValD (VarP gidName) (NormalB gidExpr) []
           ]
    _ -> fail "makeGID expects a simple variable name"

makeGIDsAndMapWithName :: String -> [ExpQ] -> Q [Dec]
makeGIDsAndMapWithName mapName expQs = do
  exps <- sequence expQs
  let numberedPairs = zip exps [1..]

  -- Generate GID declarations
  gidDeclarations <- concat <$> mapM (uncurry makeGID) numberedPairs

  -- Generate the map declaration with custom name
  mapDeclaration <- makeMapDeclarationWithName mapName numberedPairs

  pure (gidDeclarations ++ [mapDeclaration])

makeLocationGIDsAndMap :: [ExpQ] -> Q [Dec]
makeLocationGIDsAndMap = makeGIDsAndMapWithName "locationMap"

makeObjectGIDsAndMap :: [ExpQ] -> Q [Dec]
makeObjectGIDsAndMap = makeGIDsAndMapWithName "objectMap"

makeMapDeclarationWithName :: String -> [(Exp, Int)] -> Q Dec
makeMapDeclarationWithName _ [] = fail "Cannot create map from empty list"
makeMapDeclarationWithName mapNameStr numberedPairs@((firstExp, _):_) = do
  -- Get the type from the first expression
  valueType <- case firstExp of
    VarE name -> do
      info <- reify name
      case info of
        VarI _ typ _ -> extractType typ
        _            -> fail "Expected a variable"
    _ -> fail "Expected a variable expression"

  let mapName = mkName mapNameStr
      mapType = AppT (ConT ''GIDToDataMap)
                     (AppT valueType valueType)

      -- Create list of (GID, value) tuples
      tupleExps = map makeTuple numberedPairs
      listExp = ListE tupleExps

      -- GIDToDataMap (Map.fromList [...])
      mapFromListExp = AppE (VarE 'Data.Map.Strict.fromList) listExp
      gidToDataMapExp = AppE (ConE 'GIDToDataMap) mapFromListExp
  pure $ ValD (VarP mapName) (NormalB gidToDataMapExp) []

extractType :: Type -> Q Type
extractType (ForallT _ _ t)               = extractType t
extractType (AppT (AppT ArrowT _) result) = extractType result
extractType t                             = pure t
