module Build.Templates.Sentence where
import           Data.List                  (nub)
import           Data.Map.Strict            (Map, fromList)
import           Language.Haskell.TH        (Body (NormalB), Dec (SigD, ValD),
                                             Exp (AppE, ConE, ListE, LitE, TupE, VarE),
                                             ExpQ, Lit (IntegerL), Pat (VarP),
                                             Q, mkName, nameBase)
import           Model.GameState            (ImplicitStimulusActionF,
                                             ProcessImplicitStimulusVerb)
import           Model.GID                  (GID (GID))
import           Model.Parser.Atomics.Verbs (ImplicitStimulusVerb)
import           Prelude                    hiding (exp)

makeActionGID :: String -> Int -> Q [Dec]
makeActionGID actionName gidValue = do
  let gidName = mkName (actionName ++ "GID")
      gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))

  -- GID for ImplicitStimulusActionF
  gidType <- [t| GID ImplicitStimulusActionF |]

  pure [ SigD gidName gidType
       , ValD (VarP gidName) (NormalB gidExpr) []
       ]

makeProcessImplicitVerbMapsTH :: ExpQ -> Q [Dec]
makeProcessImplicitVerbMapsTH expQ = do
  exp <- expQ
  case exp of
    ListE pairExps -> do
      -- Parse the pairs: [(verb, [actionFunctions])]
      verbPairs <- mapM parseVerbPair pairExps

      -- Extract all unique action function names and number them
      let allActionNames = nub $ concatMap (map extractName . snd) verbPairs
          numberedActions = zip allActionNames [1..]

      -- Generate GID declarations for each action function
      gidDecls <- concat <$> mapM (uncurry makeActionGID) numberedActions

      -- Generate the player map: Map ImplicitStimulusVerb (GID ImplicitStimulusActionF)
      playerMapDecls <- makePlayerProcessImplicitVerbMap verbPairs

      -- Generate the global action maps: Map ImplicitStimulusVerb (Map (GID ImplicitStimulusActionF) ImplicitStimulusActionF)
      actionMapDecls <- makeProcessImplicitVerbMaps verbPairs

      pure (gidDecls ++ playerMapDecls ++ actionMapDecls)
    _ -> fail "Expected list expression in makeProcessImplicitVerbMapsTH"

-- Parse (verb, [processes]) pairs - much simpler than current extractProcessesFromPair
parseVerbPair :: Exp -> Q (Exp, [Exp])
parseVerbPair (TupE [Just verbExp, Just (ListE processExps)]) = pure (verbExp, processExps)
parseVerbPair _ = fail "Expected (verb, [processes]) tuple"

-- Extract name from expression (much simpler than your current logic)
extractName :: Exp -> String
extractName (VarE name) = nameBase name
extractName _           = error "Expected variable expression"

-- Simple GID generation (preserves the naming you need for IDE)
makeSimpleProcessGID :: String -> Int -> Q [Dec]
makeSimpleProcessGID processName gidValue = do
  let gidName = mkName (processName ++ "GID")  -- manageImplicitStimulusProcessGID
      gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))
  gidType <- [t| GID ProcessImplicitStimulusVerb |]  -- Evaluate the Q Type

  pure [ SigD gidName gidType                    -- IDE can find this!
       , ValD (VarP gidName) (NormalB gidExpr) [] -- And jump to definition!
       ]

-- Simple player map generation
makeSimplePlayerMap :: [(Exp, [Exp])] -> Q [Dec]
makeSimplePlayerMap verbPairs = do
  let mapName = mkName "playerProcessImplicitVerbMap"  -- IDE can find this!
  mapType <- [t| Map ImplicitStimulusVerb (GID ProcessImplicitStimulusVerb) |]

  let -- Create map entries: (verb, firstProcessGID)
      entries = [TupE [Just verbExp, Just (VarE (mkName (extractName (head processExps) ++ "GID")))]
                | (verbExp, processExps@(_:_)) <- verbPairs
                ]

      mapExp = AppE (VarE 'Data.Map.Strict.fromList) (ListE entries)
      typeSignature = SigD mapName mapType
      valueDeclaration = ValD (VarP mapName) (NormalB mapExp) []

  pure [typeSignature, valueDeclaration]

-- Simple process maps generation
makeSimpleProcessMaps :: [(Exp, [Exp])] -> Q [Dec]
makeSimpleProcessMaps verbPairs = do
  let mapName = mkName "processImplicitVerbMaps"     -- IDE can find this!
  mapType <- [t| Map ImplicitStimulusVerb (Map (GID ProcessImplicitStimulusVerb) ProcessImplicitStimulusVerb) |]

  let -- Create outer map entries: (verb, innerMap)
      outerEntries = [TupE [Just verbExp, Just (createInnerMap processExps)]
                     | (verbExp, processExps) <- verbPairs
                     ]

      mapExp = AppE (VarE 'Data.Map.Strict.fromList) (ListE outerEntries)
      typeSignature = SigD mapName mapType
      valueDeclaration = ValD (VarP mapName) (NormalB mapExp) []

  pure [typeSignature, valueDeclaration]
  where
    createInnerMap :: [Exp] -> Exp
    createInnerMap processExps =
      let innerEntries = [TupE [Just (VarE (mkName (extractName procExp ++ "GID"))), Just procExp]
                         | procExp <- processExps]
      in AppE (VarE 'Data.Map.Strict.fromList) (ListE innerEntries)

makePlayerProcessImplicitVerbMap :: [(Exp, [Exp])] -> Q [Dec]
makePlayerProcessImplicitVerbMap verbPairs = do
  let mapName = mkName "playerProcessImplicitVerbMap"

  -- Type: Map ImplicitStimulusVerb (GID ProcessImplicitStimulusVerb)
  mapType <- [t| Map ImplicitStimulusVerb (GID ProcessImplicitStimulusVerb) |]

  -- Create map entries: each verb maps to the first process GID
  let entries = [TupE [Just verbExp, Just (VarE (mkName (extractName (head processExps) ++ "GID")))]
                | (verbExp, processExps@(_:_)) <- verbPairs
                ]

  let mapExp = AppE (VarE 'Data.Map.Strict.fromList) (ListE entries)
      typeSignature = SigD mapName mapType
      valueDeclaration = ValD (VarP mapName) (NormalB mapExp) []

  pure [typeSignature, valueDeclaration]

makeProcessImplicitVerbMaps :: [(Exp, [Exp])] -> Q [Dec]
makeProcessImplicitVerbMaps verbPairs = do
  let mapName = mkName "processImplicitVerbMaps"

  -- Type: Map ImplicitStimulusVerb (Map (GID ImplicitStimulusActionF) ImplicitStimulusActionF)
  mapType <- [t| Map ImplicitStimulusVerb (Map (GID ImplicitStimulusActionF) ImplicitStimulusActionF) |]

  -- Create outer map entries: (verb, innerMap)
  let outerEntries = [TupE [Just verbExp, Just (createInnerMap actionExps)]
                     | (verbExp, actionExps) <- verbPairs
                     ]

  let mapExp = AppE (VarE 'Data.Map.Strict.fromList) (ListE outerEntries)
      typeSignature = SigD mapName mapType
      valueDeclaration = ValD (VarP mapName) (NormalB mapExp) []

  pure [typeSignature, valueDeclaration]
  where
    createInnerMap :: [Exp] -> Exp
    createInnerMap actionExps =
      let innerEntries = [TupE [Just (VarE (mkName (extractName actionExp ++ "GID"))),
                               Just actionExp]  -- Direct action function, no wrapper
                         | actionExp <- actionExps]
      in AppE (VarE 'Data.Map.Strict.fromList) (ListE innerEntries)
