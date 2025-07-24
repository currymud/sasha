module Grammar.Parser.Partitions.Nouns where

import           Data.HashSet               (HashSet, fromList, toList)
import           Grammar.Parser.Lexer       (HasLexeme (toLexeme),
                                             Lexeme (BED, CHAIR, LAMP, MAIL, PILL, POCKET, ROBE, SATCHEL, TABLE, TOWEL))
import           Model.Parser.Atomics.Nouns (Container (Container),
                                             DirectionalStimulus (DirectionalStimulus),
                                             Objective (Objective),
                                             Supportive (Supportive))
containers :: HashSet Container
containers =
  fromList $ map Container [SATCHEL,ROBE]
-- Nouns that can be looked at
directionalStimulii :: HashSet DirectionalStimulus
directionalStimulii = fromList $ DirectionalStimulus <$>
  lexemes objectives

-- Nouns that can have their state changed. They can be gotten, dropped, or put somewhere.
objectives :: HashSet Objective
objectives =
  fromList $ map Objective [PILL,MAIL,SATCHEL,TOWEL,ROBE]

supportives :: HashSet Supportive
supportives =
  fromList $ map Supportive [CHAIR,TABLE,SATCHEL ]

lexemes :: (HasLexeme a) => HashSet a -> [Lexeme]
lexemes speechParts = toLexeme <$> toList speechParts

