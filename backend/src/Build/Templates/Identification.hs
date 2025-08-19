{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Build.Templates.Identification where
import qualified Data.Map.Strict
import           Language.Haskell.TH      (Body (NormalB), Dec (SigD, ValD),
                                           Exp (AppE, ConE, ListE, LitE, TupE, VarE),
                                           ExpQ, Info (VarI), Lit (IntegerL),
                                           Name, Pat (VarP), Q,
                                           Type (AppT, ArrowT, ConT, ForallT),
                                           mkName, nameBase, reify)
import           Model.GameState          (AcquisitionActionF,
                                           ConsumptionActionF,
                                           DirectionalStimulusActionF,
                                           ImplicitStimulusActionF, Location,
                                           Object, PosturalActionF,
                                           ProcessImplicitStimulusVerb,
                                           SomaticAccessActionF)
import           Model.GameState.Mappings (GIDToDataMap (GIDToDataMap))
import           Model.GID                (GID (GID))
import           Model.Label              (Label (Label))
import           Model.Parser.Lexer       (Lexeme)
import           Prelude                  hiding (exp)


makeObjectGIDsFromNames :: [String] -> Q [Dec]
makeObjectGIDsFromNames names = do
  let numberedNames = zip names [1..]
  concat <$> mapM makeObjectGIDFromName numberedNames
  where
    makeObjectGIDFromName :: (String, Int) -> Q [Dec]
    makeObjectGIDFromName (name, gidValue) = do
      let gidName = mkName (name ++ "GID")
          gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))
          gidType = AppT (ConT ''GID) (ConT ''Object)
      pure [ SigD gidName gidType
           , ValD (VarP gidName) (NormalB gidExpr) []
           ]

-- =============================================================================
-- MANUAL GID ASSIGNMENT FUNCTIONS (UPDATED PATTERN)
-- =============================================================================

-- Updated: All functions now use manual GID assignment like makeAcquisitionActionGIDsAndMap

makeImplicitStimulusActionGIDsAndMap :: [(ExpQ, Int)] -> Q [Dec]
makeImplicitStimulusActionGIDsAndMap expGidPairs = do
  pairs <- mapM (\(expQ, gid) -> (,gid) <$> expQ) expGidPairs

  gidDecls <- concat <$> mapM (makeGIDForType ''ImplicitStimulusActionF) pairs
  mapDecl <- makeMapForType ''ImplicitStimulusActionF "implicitStimulusActionMap" pairs

  pure (gidDecls ++ [mapDecl])

makeDirectionalStimulusActionGIDsAndMap :: [(ExpQ, Int)] -> Q [Dec]
makeDirectionalStimulusActionGIDsAndMap expGidPairs = do
  pairs <- mapM (\(expQ, gid) -> (,gid) <$> expQ) expGidPairs

  gidDecls <- concat <$> mapM (makeGIDForType ''DirectionalStimulusActionF) pairs
  mapDecl <- makeMapForType ''DirectionalStimulusActionF "directionalStimulusActionMap" pairs

  pure (gidDecls ++ [mapDecl])

makeSomaticAccessActionGIDsAndMap :: [(ExpQ, Int)] -> Q [Dec]
makeSomaticAccessActionGIDsAndMap expGidPairs = do
  pairs <- mapM (\(expQ, gid) -> (,gid) <$> expQ) expGidPairs

  gidDecls <- concat <$> mapM (makeGIDForType ''SomaticAccessActionF) pairs
  mapDecl <- makeMapForType ''SomaticAccessActionF "somaticAccessActionMap" pairs

  pure (gidDecls ++ [mapDecl])

makeConsumptionActionGIDsAndMap :: [(ExpQ, Int)] -> Q [Dec]
makeConsumptionActionGIDsAndMap expGidPairs = do
  pairs <- mapM (\(expQ, gid) -> (,gid) <$> expQ) expGidPairs

  gidDecls <- concat <$> mapM (makeGIDForType ''ConsumptionActionF) pairs
  mapDecl <- makeMapForType ''ConsumptionActionF "consumptionActionMap" pairs

  pure (gidDecls ++ [mapDecl])

makePosturalActionGIDsAndMap :: [(ExpQ, Int)] -> Q [Dec]
makePosturalActionGIDsAndMap expGidPairs = do
  pairs <- mapM (\(expQ, gid) -> (,gid) <$> expQ) expGidPairs

  gidDecls <- concat <$> mapM (makeGIDForType ''PosturalActionF) pairs
  mapDecl <- makeMapForType ''PosturalActionF "posturalActionMap" pairs

  pure (gidDecls ++ [mapDecl])

-- Keep the existing makeAcquisitionActionGIDsAndMap as is (already uses manual GID assignment)
makeAcquisitionActionGIDsAndMap :: [(ExpQ, Int)] -> Q [Dec]
makeAcquisitionActionGIDsAndMap expGidPairs = do
  pairs <- mapM (\(expQ, gid) -> (,gid) <$> expQ) expGidPairs

  gidDecls <- concat <$> mapM (makeGIDForType ''AcquisitionActionF) pairs
  mapDecl <- makeMapForType ''AcquisitionActionF "acquisitionActionMap" pairs

  pure (gidDecls ++ [mapDecl])

-- =============================================================================
-- CORE HELPER FUNCTIONS
-- =============================================================================
  {-
makeGIDForType :: Name -> (Exp, Int) -> Q [Dec]
makeGIDForType typeName (VarE name, gidValue) = do
  let gidName = mkName (nameBase name ++ "GID")
      gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))
      gidType = AppT (ConT ''GID) (ConT typeName)

  pure [ SigD gidName gidType
       , ValD (VarP gidName) (NormalB gidExpr) []
       ]
makeGIDForType _ _ = fail "Expected variable"
-}

makeGIDForType :: Name -> (Exp, Int) -> Q [Dec]
makeGIDForType typeName (exp, gidValue) = do
  case exp of
    VarE name -> do
      let gidName = mkName (nameBase name ++ "GID")
          gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))
          gidType = AppT (ConT ''GID) (ConT typeName)

      pure [ SigD gidName gidType
           , ValD (VarP gidName) (NormalB gidExpr) []
           ]
    _ -> fail $ "Expected variable, got: " ++ show exp

makeMapForType :: Name -> String -> [(Exp, Int)] -> Q Dec
makeMapForType typeName mapName pairs = do
  let mapNameQ = mkName mapName

      tuples = [TupE [Just (VarE (mkName (nameBase name ++ "GID"))), Just (VarE name)]
               | (VarE name, _) <- pairs]

      listExp = ListE tuples
      mapExp = AppE (VarE 'Data.Map.Strict.fromList) listExp

  pure $ ValD (VarP mapNameQ) (NormalB mapExp) []

-- =============================================================================
-- INDIVIDUAL GID HELPER FUNCTIONS (FOR CONSISTENCY)
-- =============================================================================

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

makeSomaticAccessActionGID :: Exp -> Int -> Q [Dec]
makeSomaticAccessActionGID exp gidValue = do
  case exp of
    VarE functionName -> do
      let originalNameStr = nameBase functionName
          gidNameStr = originalNameStr ++ "GID"
          gidName = mkName gidNameStr
          gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))
          gidType = AppT (ConT ''GID) (ConT ''SomaticAccessActionF)

      pure [ SigD gidName gidType
           , ValD (VarP gidName) (NormalB gidExpr) []
           ]
    _ -> fail "makeSomaticAccessActionGID expects a simple variable name"

makeConsumptionActionGID :: Exp -> Int -> Q [Dec]
makeConsumptionActionGID exp gidValue = do
  case exp of
    VarE functionName -> do
      let originalNameStr = nameBase functionName
          gidNameStr = originalNameStr ++ "GID"
          gidName = mkName gidNameStr
          gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))
          gidType = AppT (ConT ''GID) (ConT ''ConsumptionActionF)

      pure [ SigD gidName gidType
           , ValD (VarP gidName) (NormalB gidExpr) []
           ]
    _ -> fail "makeConsumptionActionGID expects a simple variable name"

makePosturalActionGID :: Exp -> Int -> Q [Dec]
makePosturalActionGID exp gidValue = do
  case exp of
    VarE functionName -> do
      let originalNameStr = nameBase functionName
          gidNameStr = originalNameStr ++ "GID"
          gidName = mkName gidNameStr
          gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))
          gidType = AppT (ConT ''GID) (ConT ''PosturalActionF)

      pure [ SigD gidName gidType
           , ValD (VarP gidName) (NormalB gidExpr) []
           ]
    _ -> fail "makePosturalActionGID expects a simple variable name"

makeAcquisitionActionGID :: Exp -> Int -> Q [Dec]
makeAcquisitionActionGID exp' gidValue = do
  case exp' of
    VarE functionName -> do
      let originalNameStr = nameBase functionName
          gidNameStr = originalNameStr ++ "GID"
          gidName = mkName gidNameStr
          gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))
          gidType = AppT (ConT ''GID) (ConT ''AcquisitionActionF)

      pure [ SigD gidName gidType
           , ValD (VarP gidName) (NormalB gidExpr) []
           ]
    _ -> fail "makeAcquisitionActionGID expects a simple variable name"

-- =============================================================================
-- EXISTING FUNCTIONS (KEPT UNCHANGED)
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
-- LOCATION AND OBJECT MAP FUNCTIONS (KEPT UNCHANGED)
-- =============================================================================

makeLocationGIDsAndMap :: [ExpQ] -> Q [Dec]
makeLocationGIDsAndMap = makeGIDsAndMapForType ''Location "locationMap"

makeObjectGIDsAndMap :: [ExpQ] -> Q [Dec]
makeObjectGIDsAndMap = makeGIDsAndMapForType ''Object "objectMap"

makeGIDsAndMapForType :: Name -> String -> [ExpQ] -> Q [Dec]
makeGIDsAndMapForType typeName mapName expQs = do
  exps <- sequence expQs
  let pairs = zip exps [1..]

  gidDecls <- concat <$> mapM (makeGIDForType typeName) pairs
  mapDecl <- makeMapForType typeName mapName pairs

  pure (gidDecls ++ [mapDecl])

-- =============================================================================
-- LABEL FUNCTIONS (KEPT UNCHANGED)
-- =============================================================================

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
