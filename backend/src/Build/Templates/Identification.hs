module Build.Templates.Identification where
import           Data.Char                  (toLower)
import           Language.Haskell.TH.Lib    (DecsQ, ExpQ)
import           Language.Haskell.TH.Syntax (Body (NormalB), Dec (SigD, ValD),
                                             Exp (AppE, ConE, LitE, UnboundVarE, VarE),
                                             Lit (IntegerL), Pat (VarP), Q,
                                             Type (AppT, ConT), mkName,
                                             nameBase)
import           Model.GameState            (ActionF, ResolutionF (ResolutionF))
import           Model.GID                  (GID (GID))
import           Model.Parser.Lexer         (Lexeme)
import           Prelude                    hiding (exp)

labelTemplate :: String -> String -> Lexeme -> DecsQ
labelTemplate typeStr binding lexeme = pure desc
  where
    desc        = [sigd,vald]
    sigd        = SigD decLabel appt
    appt        = AppT labelType' objectType'
    decLabel    = mkName (binding <> "Label")
    labelType'  = ConT labelName
    objectType' = ConT (mkName typeStr)
    vald = ValD (VarP decLabel) eval' []
    eval' = NormalB (AppE (ConE labelName) uvar)
    uvar = UnboundVarE $ (mkName . show) lexeme
    labelName = mkName "Game.Model.Label.Label"

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

makeActionGID :: ExpQ -> Int -> Q [Dec]
makeActionGID expQ gidValue = do
  exp <- expQ
  case exp of
    VarE functionName -> do
      let -- Extract the string name and add '
          originalNameStr = nameBase functionName
          gidNameStr = originalNameStr ++ "'"
          gidName = mkName gidNameStr

          -- Create the GID constructor expression
          gidExpr = AppE (ConE 'GID) (LitE (IntegerL (fromIntegral gidValue)))

          -- Create type signature: GID ActionF
          gidType = AppT (ConT ''GID) (AppT (ConT ''ActionF) (ConT ''ResolutionF))

      pure [ SigD gidName gidType
             , ValD (VarP gidName) (NormalB gidExpr) []
             ]
    _ -> fail "makeActionGID expects a simple variable name"

makeActionGIDsAuto :: [ExpQ] -> Q [Dec]
makeActionGIDsAuto expQs = do
  let numberedPairs = zip expQs [1..]
  concat <$> mapM (uncurry makeActionGID) numberedPairs

makeVerbValue :: Lexeme -> ExpQ -> Q [Dec]
makeVerbValue lexeme constructorExpQ = do
  constructorExp <- constructorExpQ
  case constructorExp of
    ConE constructorName -> do
      let -- Convert lexeme to lowercase for value name
          lexemeStr = show lexeme
          valueName = mkName (map toLower lexemeStr)

          -- Extract constructor type name (assumes constructor is same as type)
          constructorTypeStr = nameBase constructorName
          constructorTypeName = mkName constructorTypeStr

          -- Create type signature: valueName :: ConstructorType
          typeSignature = SigD valueName (ConT constructorTypeName)

          -- Create value declaration: valueName = Constructor LEXEME
          valueDeclaration = ValD (VarP valueName)
                                  (NormalB (AppE (ConE constructorName)
                                                 (ConE (mkName lexemeStr))))
                                  []
      pure [typeSignature, valueDeclaration]
    _ -> fail "makeVerbValue expects a constructor expression"


makeVerbValues :: ExpQ -> [Lexeme] -> Q [Dec]
makeVerbValues constructorExpQ lexemes = do
  declarations <- mapM (`makeVerbValue` constructorExpQ) lexemes
  pure (concat declarations)
