module Build.Templates.Identification where
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict
import           Language.Haskell.TH        (reify)
import           Language.Haskell.TH.Lib    (DecsQ, ExpQ)
import           Language.Haskell.TH.Syntax (Body (NormalB), Dec (SigD, ValD),
                                             Exp (AppE, ConE, ListE, LitE, TupE, UnboundVarE, VarE),
                                             Info (VarI), Lit (IntegerL), Name,
                                             Pat (VarP), Q,
                                             Type (AppT, ArrowT, ConT, ForallT),
                                             mkName, nameBase)
import           Model.GameState            (ActionF, ActionMap,
                                             ProcessImplicitStimulusVerb,
                                             ProcessImplicitVerbMaps)
import           Model.GID                  (GID (GID))
import           Model.Mappings             (GIDToDataMap (GIDToDataMap))
import           Model.Parser.Atomics.Verbs (ImplicitStimulusVerb)
import           Model.Parser.Lexer         (Lexeme)
import           Prelude                    hiding (exp)

-- =============================================================================
-- CORE HELPER FUNCTIONS
-- =============================================================================

-- | Generic GID creation helper for function names with specific type
makeGIDHelperWithType :: Name -> Name -> Int -> Q [Dec]
makeGIDHelperWithType functionName typeName gidValue = do
  let originalNameStr = nameBase functionName
      gidNameStr = originalNameStr ++ "GID"
      gidName = mkName gidNameStr
      gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))
      gidType = AppT (ConT ''GID) (ConT typeName)

  pure [ SigD gidName gidType
       , ValD (VarP gidName) (NormalB gidExpr) []
       ]

-- | Create GID variable name from expression
gidNameFromExp :: Exp -> Name
gidNameFromExp (VarE functionName) =
  mkName (nameBase functionName ++ "GID")
gidNameFromExp _ = error "Expected VarE in gidNameFromExp"

-- | Create tuple for map entries
makeTupleHelper :: (Exp, Int) -> Exp
makeTupleHelper (exp, _) =
  TupE [Just (VarE (gidNameFromExp exp)), Just exp]

-- | Extract type from complex type signatures
extractType :: Type -> Q Type
extractType (ForallT _ _ t)               = extractType t
extractType (AppT (AppT ArrowT _) result) = extractType result
extractType t                             = pure t

-- | Efficient duplicate removal using manual comparison
removeDuplicatesEfficient :: [Exp] -> [Exp]
removeDuplicatesEfficient = go []
  where
    go seen [] = reverse seen
    go seen (x:xs)
      | x `elem` seen = go seen xs
      | otherwise = go (x:seen) xs

-- =============================================================================
-- LABEL CREATION
-- =============================================================================

labelTemplate :: Exp -> Lexeme -> DecsQ
labelTemplate exp lexeme = do
  case exp of
    VarE varName -> do
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
      pairs = zip exps lexemes

  declarations <- mapM (uncurry labelTemplate) pairs
  pure (concat declarations)

-- =============================================================================
-- ACTION MAP CREATION
-- =============================================================================

makeActionGIDsAndMap :: [ExpQ] -> Q [Dec]
makeActionGIDsAndMap expQs = do
  exps <- sequence expQs
  let numberedPairs = zip exps [1..]

  -- Generate GID declarations
  gidDeclarations <- concat <$> mapM (uncurry makeActionGID) numberedPairs

  -- Generate the ActionMap declaration
  mapDeclarations <- makeActionMapDeclaration numberedPairs

  pure (gidDeclarations ++ mapDeclarations)

makeActionMapDeclaration :: [(Exp, Int)] -> Q [Dec]
makeActionMapDeclaration [] = fail "Cannot create ActionMap from empty list"
makeActionMapDeclaration numberedPairs = do
  let mapName = mkName "actionMap"
      actionMapType = ConT ''ActionMap
      typeSignature = SigD mapName actionMapType

  valueDeclaration <- makeMapDeclarationWithName "actionMap" numberedPairs
  pure [typeSignature, valueDeclaration]

makeActionGID :: Exp -> Int -> Q [Dec]
makeActionGID exp gidValue = do
  case exp of
    VarE functionName -> do
      let originalNameStr = nameBase functionName
          gidNameStr = originalNameStr ++ "GID"
          gidName = mkName gidNameStr
          gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))
          -- ActionF is a concrete type, not derived from function name
          gidType = AppT (ConT ''GID) (ConT ''ActionF)

      pure [ SigD gidName gidType
           , ValD (VarP gidName) (NormalB gidExpr) []
           ]
    _ -> fail "makeActionGID expects a simple variable name"

-- =============================================================================
-- GENERIC MAP CREATION
-- =============================================================================

makeGIDsAndMapWithName :: String -> [ExpQ] -> Q [Dec]
makeGIDsAndMapWithName mapName expQs = do
  exps <- sequence expQs
  let numberedPairs = zip exps [1..]

  -- Generate GID declarations
  gidDeclarations <- concat <$> mapM (uncurry makeGID) numberedPairs

  -- Generate the map declaration
  mapDeclaration <- makeMapDeclarationWithName mapName numberedPairs

  pure (gidDeclarations ++ [mapDeclaration])

makeLocationGIDsAndMap :: [ExpQ] -> Q [Dec]
makeLocationGIDsAndMap = makeGIDsAndMapWithName "locationMap"

makeObjectGIDsAndMap :: [ExpQ] -> Q [Dec]
makeObjectGIDsAndMap = makeGIDsAndMapWithName "objectMap"

makeGID :: Exp -> Int -> Q [Dec]
makeGID exp gidValue = do
  case exp of
    VarE name -> do
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
      mapType = AppT (ConT ''GIDToDataMap) (AppT valueType valueType)
      tupleExps = map makeTupleHelper numberedPairs
      listExp = ListE tupleExps
      mapFromListExp = AppE (VarE 'Data.Map.Strict.fromList) listExp
      gidToDataMapExp = AppE (ConE 'GIDToDataMap) mapFromListExp

  pure $ ValD (VarP mapName) (NormalB gidToDataMapExp) []

-- =============================================================================
-- PROCESS IMPLICIT VERB MAP CREATION
-- =============================================================================

-- | Creates GIDs and both ProcessImplicitVerbMap and PlayerProcessImplicitVerbMap
makeProcessImplicitVerbMapsTH :: ExpQ -> Q [Dec]
makeProcessImplicitVerbMapsTH expQ = do
  exp <- expQ
  case exp of
    ListE pairExps -> do
      -- Extract all unique processes from all pairs
      processes <- extractProcesses pairExps
      let numberedProcesses = zip processes [1..]

      -- Create GID declarations for each process
      processGidDeclarations <- concat <$> mapM (uncurry makeProcessGID) numberedProcesses

      -- Create both maps
      regularMapDecls <- makeProcessImplicitVerbMapDeclaration pairExps numberedProcesses
      playerMapDecls <- makePlayerProcessImplicitVerbMapDeclaration pairExps numberedProcesses

      pure (processGidDeclarations ++ regularMapDecls ++ playerMapDecls)
    _ -> fail "makeProcessImplicitVerbMapTH expects a list expression"

-- | Extract all unique processes from pair expressions (efficient version)
extractProcesses :: [Exp] -> Q [Exp]
extractProcesses pairExps = do
  allProcesses <- concat <$> mapM extractProcessesFromPair pairExps
  pure $ removeDuplicatesEfficient allProcesses

-- | Extract processes from a single pair expression
extractProcessesFromPair :: Exp -> Q [Exp]
extractProcessesFromPair (TupE [Just _, Just (ListE processes)]) = pure processes
extractProcessesFromPair _ = fail "Expected tuple with (verb, [processes])"

-- | Create GID declaration for a process
makeProcessGID :: Exp -> Int -> Q [Dec]
makeProcessGID exp gidValue = do
  case exp of
    VarE functionName -> do
      let originalNameStr = nameBase functionName
          gidNameStr = originalNameStr ++ "GID"
          gidName = mkName gidNameStr
          gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))
          gidType = AppT (ConT ''GID) (ConT ''ProcessImplicitStimulusVerb)

      pure [ SigD gidName gidType
           , ValD (VarP gidName) (NormalB gidExpr) []
           ]
    _ -> fail "makeProcessGID expects a simple variable name"

-- =============================================================================
-- MAIN PROCESS MAP (ProcessImplicitVerbMaps)
-- =============================================================================

-- | Create the ProcessImplicitVerbMaps declaration (note: plural, the main outer map)
makeProcessImplicitVerbMapDeclaration :: [Exp] -> [(Exp, Int)] -> Q [Dec]
makeProcessImplicitVerbMapDeclaration pairExps numberedProcesses = do
  let mapName = mkName "processImplicitVerbMaps"
      -- This should be ProcessImplicitVerbMaps (plural) - the outer map type
      mapType = ConT ''ProcessImplicitVerbMaps
      typeSignature = SigD mapName mapType

  -- Create the nested map structure
  pairExprs <- mapM (createMapPairExpression numberedProcesses) pairExps
  let outerListExp = ListE pairExprs
      outerMapExp = AppE (VarE 'Data.Map.Strict.fromList) outerListExp
      valueDeclaration = ValD (VarP mapName) (NormalB outerMapExp) []

  pure [typeSignature, valueDeclaration]

-- | Create a single pair expression for the outer map: (verb, innerMap)
-- Creates: (ImplicitStimulusVerb, ProcessImplicitVerbMap)
createMapPairExpression :: [(Exp, Int)] -> Exp -> Q Exp
createMapPairExpression numberedProcesses (TupE [Just verbExp, Just (ListE processes)]) = do
  -- Create inner map from processes: Map (GID ProcessImplicitStimulusVerb) ProcessImplicitStimulusVerb
  innerPairs <- mapM (createInnerMapPair numberedProcesses) processes
  let innerListExp = ListE innerPairs
      innerMapExp = AppE (VarE 'Data.Map.Strict.fromList) innerListExp

  pure $ TupE [Just verbExp, Just innerMapExp]
createMapPairExpression _ _ = fail "Expected tuple with (verb, [processes]) in regular map"

-- | Create a pair for the inner map: (processGID, process)
-- Creates: (GID ProcessImplicitStimulusVerb, ProcessImplicitStimulusVerb)
createInnerMapPair :: [(Exp, Int)] -> Exp -> Q Exp
createInnerMapPair _numberedProcesses processExp = do
  case processExp of
    VarE processName -> do
      let gidName = mkName (nameBase processName ++ "GID")
      pure $ TupE [Just (VarE gidName), Just processExp]
    _ -> fail "Expected process variable in inner map"

-- =============================================================================
-- PLAYER PROCESS MAP
-- =============================================================================

-- | Create the PlayerProcessImplicitVerbMap declaration
makePlayerProcessImplicitVerbMapDeclaration :: [Exp] -> [(Exp, Int)] -> Q [Dec]
makePlayerProcessImplicitVerbMapDeclaration pairExps numberedProcesses = do
  let mapName = mkName "playerProcessImplicitVerbMap"
      -- Construct the type explicitly: Map ImplicitStimulusVerb (GID ProcessImplicitStimulusVerb)
      mapType = AppT (AppT (ConT ''Map) (ConT ''ImplicitStimulusVerb))
                     (AppT (ConT ''GID) (ConT ''ProcessImplicitStimulusVerb))
      typeSignature = SigD mapName mapType

  -- Create the map structure - each verb maps to the first process GID
  pairExprs <- mapM (createPlayerMapPairExpression numberedProcesses) pairExps
  let outerListExp = ListE pairExprs
      outerMapExp = AppE (VarE 'Data.Map.Strict.fromList) outerListExp
      valueDeclaration = ValD (VarP mapName) (NormalB outerMapExp) []

  pure [typeSignature, valueDeclaration]

-- | Create a single pair expression for the player map: (verb, firstProcessGID)
createPlayerMapPairExpression :: [(Exp, Int)] -> Exp -> Q Exp
createPlayerMapPairExpression _numberedProcesses (TupE [Just verbExp, Just (ListE processes)]) = do
  case processes of
    [] -> fail "Empty process list in verb mapping"
    (firstProcess:_) -> do
      firstProcessGID <- createPlayerMapProcessGID firstProcess
      pure $ TupE [Just verbExp, Just firstProcessGID]
createPlayerMapPairExpression _ _ = fail "Expected tuple with (verb, [processes]) in player map"

-- | Get the GID variable expression for a process in the player map
createPlayerMapProcessGID :: Exp -> Q Exp
createPlayerMapProcessGID processExp = do
  case processExp of
    VarE processName -> do
      let gidName = mkName (nameBase processName ++ "GID")
      pure $ VarE gidName
    _ -> fail "Expected process variable in player map"

-- =============================================================================
-- LEGACY/UNUSED FUNCTIONS (kept for backward compatibility)
-- =============================================================================

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
