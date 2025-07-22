module Build.Templates.Sentence where
import           Data.List                  (nub)
import           Data.Map.Strict            (Map, fromList)
import           Language.Haskell.TH        (Body (NormalB), Dec (SigD, ValD),
                                             Exp (AppE, ConE, ListE, LitE, TupE, VarE),
                                             ExpQ, Lit (IntegerL), Pat (VarP),
                                             Q, mkName, nameBase)
import           Model.GameState            (ProcessImplicitStimulusVerb)
import           Model.GID                  (GID (GID))
import           Model.Parser.Atomics.Verbs (ImplicitStimulusVerb)
import           Prelude                    hiding (exp)

-- =============================================================================
-- SIMPLIFIED TH THAT PRESERVES IDE NAVIGATION
-- =============================================================================

-- The key insight: You need the NAMES for IDE support, but you can simplify
-- the GENERATION logic dramatically.

-- =============================================================================
-- APPROACH 1: SIMPLIFIED TH - SAME INTERFACE, MUCH SIMPLER LOGIC
-- =============================================================================

-- Replace your 200+ line monster with this 30-line version:
-- SAME INTERFACE: makeProcessImplicitVerbMapsSimple :: ExpQ -> Q [Dec]
makeProcessImplicitVerbMapsTH :: ExpQ -> Q [Dec]
makeProcessImplicitVerbMapsTH expQ = do
  exp <- expQ
  case exp of
    ListE pairExps -> do
      -- Parse the pairs (much simpler than current)
      verbPairs <- mapM parseVerbPair pairExps

      -- Extract all unique process names
      let allProcessNames = nub $ concatMap (map extractName . snd) verbPairs
          numberedProcesses = zip allProcessNames [1..]

      -- Generate GID declarations (preserves IDE navigation)
      gidDecls <- concat <$> mapM (uncurry makeSimpleProcessGID) numberedProcesses

      -- Generate the maps (much simpler than current)
      playerMapDecls <- makeSimplePlayerMap verbPairs
      processMapDecls <- makeSimpleProcessMaps verbPairs

      pure (gidDecls ++ playerMapDecls ++ processMapDecls)
    _ -> fail "Expected list expression"

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

-- =============================================================================
-- USAGE - EXACTLY THE SAME AS BEFORE!
-- =============================================================================

-- Your current usage:
-- makeProcessImplicitVerbMapsTH [| [ (look, [manageImplicitStimulusProcess]) ] |]

-- New usage (IDENTICAL):
-- makeProcessImplicitVerbMapsSimple [| [ (look, [manageImplicitStimulusProcess]) ] |]

-- Just change the function name, everything else stays the same!

