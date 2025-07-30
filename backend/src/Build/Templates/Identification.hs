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
import           Model.GameState            (DirectionalStimulusActionF,
                                             ImplicitStimulusActionF, Location,
                                             Object,
                                             PlayerProcessImplicitVerbMap,
                                             ProcessImplicitStimulusVerb,
                                             ProcessImplicitVerbMaps,
                                             SomaticAccessActionF)
import           Model.GID                  (GID (GID))
import           Model.Label                (Label (Label))
import           Model.Mappings             (GIDToDataMap (GIDToDataMap))
import           Model.Parser.Atomics.Verbs (DirectionalStimulusVerb,
                                             ImplicitStimulusVerb)
import           Model.Parser.Lexer         (Lexeme)
import           Prelude                    hiding (exp)

makeLabels :: [(ExpQ, Lexeme)] -> Q [Dec]
makeLabels expLexemePairs = do
  exps <- mapM fst expLexemePairs
  let lexemes = map snd expLexemePairs
      pairs = zip exps lexemes

  concat <$> mapM (uncurry labelDecl) pairs
  where
    labelDecl :: Exp -> Lexeme -> Q [Dec]
    labelDecl (VarE varName) lexeme = do
      let labelName = mkName (nameBase varName ++ "'")

      info <- reify varName
      baseType <- case info of
        VarI _ typ _ -> pure $ simplifyType typ
        _            -> fail "Expected variable"

      -- CONSTRUCT DECLARATIONS
      let lexemeExpr = ConE (mkName (show lexeme))
          labelExpr = AppE (ConE 'Label) lexemeExpr
          labelType = AppT (ConT ''Label) baseType

      pure [ SigD labelName labelType
           , ValD (VarP labelName) (NormalB labelExpr) []
           ]
    labelDecl _ _ = fail "Expected variable"

-- Much simpler type simplification (handles 90% of cases)
simplifyType :: Type -> Type
simplifyType (ForallT _ _ t)               = simplifyType t           -- Remove forall
simplifyType (AppT (AppT ArrowT _) result) = result     -- a -> Location becomes Location
simplifyType (ConT name)                   = ConT name                    -- Location stays Location
simplifyType t                             = t                                      -- Everything else unchanged

makeLocationGIDsAndMap :: [ExpQ] -> Q [Dec]
makeLocationGIDsAndMap = makeGIDsAndMapForType ''Location "locationMap"

makeObjectGIDsAndMap :: [ExpQ] -> Q [Dec]
makeObjectGIDsAndMap = makeGIDsAndMapForType ''Object "objectMap"

-- Updated for new action system

makeImplicitStimulusActionGIDsAndMap :: [ExpQ] -> Q [Dec]
makeImplicitStimulusActionGIDsAndMap = makeGIDsAndMapForType ''ImplicitStimulusActionF "implicitStimulusActionMap"

makeDirectionalStimulusActionGIDsAndMap :: [ExpQ] -> Q [Dec]
makeDirectionalStimulusActionGIDsAndMap = makeGIDsAndMapForType ''DirectionalStimulusActionF "directionalStimulusActionMap"

makeSomaticAccessActionGIDsAndMap :: [ExpQ] -> Q [Dec]
makeSomaticAccessActionGIDsAndMap = makeGIDsAndMapForType ''SomaticAccessActionF "somaticAccessActionMap"

makeGIDsAndMapForType :: Name -> String -> [ExpQ] -> Q [Dec]
makeGIDsAndMapForType typeName mapName expQs = do
  exps <- sequence expQs
  let pairs = zip exps [1..]

  gidDecls <- concat <$> mapM (makeGIDForType typeName) pairs
  mapDecl <- makeMapForType typeName mapName pairs

  pure (gidDecls ++ [mapDecl])

makeGIDForType :: Name -> (Exp, Int) -> Q [Dec]
makeGIDForType typeName (VarE name, gidValue) = do
  let gidName = mkName (nameBase name ++ "GID")
      gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))
      gidType = AppT (ConT ''GID) (ConT typeName)

  pure [ SigD gidName gidType
       , ValD (VarP gidName) (NormalB gidExpr) []
       ]
makeGIDForType _ _ = fail "Expected variable"

makeMapForType :: Name -> String -> [(Exp, Int)] -> Q Dec
makeMapForType typeName mapName pairs = do
  let mapNameQ = mkName mapName
      mapType = AppT (ConT typeName) (ConT typeName)  -- This line looks wrong

      tuples = [TupE [Just (VarE (mkName (nameBase name ++ "GID"))), Just (VarE name)]
               | (VarE name, _) <- pairs]

      listExp = ListE tuples
      mapExp = AppE (VarE 'Data.Map.Strict.fromList) listExp

  pure $ ValD (VarP mapNameQ) (NormalB mapExp) []
-- =============================================================================
-- CORE HELPER FUNCTIONS
-- =============================================================================

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

-- Updated for new action system
makeImplicitStimulusActionGID :: Exp -> Int -> Q [Dec]
makeImplicitStimulusActionGID exp gidValue = do
  case exp of
    VarE functionName -> do
      let originalNameStr = nameBase functionName
          gidNameStr = originalNameStr ++ "GID"
          gidName = mkName gidNameStr
          gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))
          gidType = AppT (ConT ''GID) (ConT ''ImplicitStimulusActionF)

      pure [ SigD gidName gidType
           , ValD (VarP gidName) (NormalB gidExpr) []
           ]
    _ -> fail "makeImplicitStimulusActionGID expects a simple variable name"

makeDirectionalStimulusActionGID :: Exp -> Int -> Q [Dec]
makeDirectionalStimulusActionGID exp gidValue = do
  case exp of
    VarE functionName -> do
      let originalNameStr = nameBase functionName
          gidNameStr = originalNameStr ++ "GID"
          gidName = mkName gidNameStr
          gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))
          gidType = AppT (ConT ''GID) (ConT ''DirectionalStimulusActionF)

      pure [ SigD gidName gidType
           , ValD (VarP gidName) (NormalB gidExpr) []
           ]
    _ -> fail "makeDirectionalStimulusActionGID expects a simple variable name"

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
