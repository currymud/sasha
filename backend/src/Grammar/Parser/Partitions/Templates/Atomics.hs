module Grammar.Parser.Partitions.Templates.Atomics where
import           Data.Char            (toLower)
import           Grammar.Parser.Lexer (Lexeme)
import           Language.Haskell.TH  (Body (NormalB), Dec (SigD, ValD),
                                       Exp (AppE, ConE), ExpQ, Pat (VarP), Q,
                                       Type (ConT), mkName, nameBase)

makeSemanticValue :: Lexeme -> ExpQ -> Q [Dec]
makeSemanticValue lexeme constructorExpQ = do
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


makeSemanticValues :: ExpQ -> [Lexeme] -> Q [Dec]
makeSemanticValues constructorExpQ lexemes = do
  declarations <- mapM (`makeSemanticValue` constructorExpQ) lexemes
  pure (concat declarations)

