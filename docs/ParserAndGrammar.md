# Sasha Parser and Grammar: Case Grammar for Domain-Specific Commands

## Introduction

Sasha's parser implements a **case grammar** system that transforms interactive fiction commands like "get robe from chair" into type-safe Haskell data structures. Sasha defines a domain-specific language (DSL) for text adventure commands, using case grammar principles to structure this DSL systematically around game actions and entities.

## What is Case Grammar?

Case grammar, developed by linguist Charles Fillmore, analyzes sentences in terms of **cases** - the semantic relationships between verbs and their associated noun phrases. Rather than focusing on surface syntax (subject, object, predicate), case grammar identifies the underlying semantic roles:

- **Agent**: Who performs the action (always the player in Sasha)
- **Patient/Object**: What receives the action ("get **the robe**")  
- **Source**: Where something comes from ("from **the chair**")
- **Instrument**: What is used to perform the action ("with **the key**")

For interactive fiction, this provides a systematic way to structure command languages around actions (verbs) and the entities (nouns) that participate in those actions in specific semantic roles.

## Architecture Overview

The parser consists of two main directories that work together:

```
Model/Parser/     # Type definitions for parsed command structures
Grammar/Parser/   # Parsing rules and lexical analysis
```

### The Pipeline: Domain Commands → Lexemes → Case Structures → Action Keys

```haskell
-- From TopLevel.hs:64-74
trySentence :: Text -> Either Text Sentence
trySentence input = case lexify tokens input of
  Left err      -> Left ("Lexeme fubar " <> err)
  Right lexemes -> case trySentence' lexemes of
    Left err       -> Left ("Parser fubar " <> err)  
    Right sentence -> Right sentence
```

1. **Lexical Analysis**: `"get robe from chair"` → `[GET, ROBE, FROM, CHAIR]`
2. **Case Grammar Parsing**: Lexemes → `AcquisitionVerbPhrase get robePhrase from chairPhrase`
3. **Action Key Generation**: Case structure → `AVManagementKey get someGID`

## Domain-Specific Lexical Foundation

### Controlled Vocabulary
```haskell
-- From Model/Parser/Lexer.hs:19-212
data Lexeme
  = GET | TAKE | OPEN | CLOSE
  | ROBE | CHAIR | PILL | EYES
  | FROM | WITH | ON | IN
  -- ... 200+ lexemes total
```

Sasha defines a **closed vocabulary** of exactly the words it understands, focusing specifically on interactive fiction concepts:

- **Game actions**: GET, TAKE, OPEN, CLOSE, LOOK, EAT
- **Game objects**: ROBE, CHAIR, PILL, EYES, TABLE
- **Spatial relationships**: FROM, ON, IN, WITH, UNDER

This provides:
- **Domain focus**: Concepts specifically relevant to the game world
- **Predictable parsing**: Clear boundaries for valid commands
- **Type safety**: Immediate feedback on vocabulary usage

### Domain-Specific Tokenization
```haskell
-- From Grammar/Parser/Lexer.hs:255-262
get :: Lexer Lexeme  
get = GET <$ symbol "GET"

close :: Lexer Lexeme
close = CLOSE <$ symbol "CLOSE" <|> CLOSE <$ symbol "SHUT"
```

The lexer maps domain vocabulary to semantic tokens, sometimes providing synonyms ("close"/"shut") while maintaining focus on interactive fiction commands.

## Semantic Types: Case Roles as Domain Concepts

Case grammar's semantic roles become domain-specific Haskell types:

```haskell
-- From Model/Parser/Atomics/Verbs.hs
newtype AcquisitionVerb = AcquisitionVerb { _fromAcquisitionVerb :: Lexeme }
newtype SomaticAccessVerb = SomaticAccessVerb { _fromSomaticAccessVerb :: Lexeme }

-- From Model/Parser/Atomics/Nouns.hs  
newtype Objective = Objective { _fromObjective :: Lexeme }
newtype Container = Container { _fromContainer :: Lexeme }
newtype SomaticStimulus = SomaticStimulus { _fromSomaticStimulus :: Lexeme }
```

Each type represents a **domain-specific semantic category**:
- `AcquisitionVerb`: Actions for taking objects (`GET`, `TAKE`)
- `SomaticAccessVerb`: Body-related actions (`OPEN` eyes, `CLOSE` eyes)
- `Objective`: Things that can be acquired (`ROBE`, `PILL`)
- `Container`: Things that can hold other things (`POCKET`, `BAG`)

This approach models the interactive fiction domain through case grammar principles.

## Case Frames: Domain Action Patterns

Case grammar organizes verbs into **case frames** - patterns that specify which semantic roles can appear with which verbs. Sasha encodes these as domain-specific command structures:

```haskell
-- From Model/Parser/Composites/Verbs.hs:27-35
data AcquisitionVerbPhrase
  = SimpleAcquisitionVerbPhrase AcquisitionVerb ObjectPhrase
  | AcquisitionVerbPhrase 
      AcquisitionVerb      -- The acquisition action
      ObjectPhrase         -- What to acquire (Patient role)
      SourceMarker         -- "from" 
      SupportPhrase        -- Where it's currently located (Source role)
```

This represents the **domain-specific case frame for acquisition commands**:
- **Core pattern**: `VERB OBJECT` ("get robe")
- **Extended pattern**: `VERB OBJECT SOURCE_MARKER LOCATION` ("get robe from chair")

### More Domain Case Frames
```haskell
-- Container access commands
data ContainerAccessVerbPhrase
  = SimpleAccessContainerVerbPhrase SimpleAccessVerb ContainerPhrase
  | ContainerAccessVerbPhrase SimpleAccessVerb ContainerPhrase InstrumentalAccessNounPhrase

-- Body-related commands  
data StimulusVerbPhrase
  = SomaticStimulusVerbPhrase SomaticAccessVerb SomaticStimulusNounPhrase
```

Each case frame captures a different **domain action pattern**:
- Container access: "open cabinet [with key]"
- Somatic actions: "open eyes", "close eyes"

## Template Haskell for Semantic Values

Rather than manually creating constructors, Sasha uses Template Haskell to generate them from lexeme lists:

```haskell
-- From Grammar/Parser/Partitions/Verbs/AcquisitionVerbs.hs:17-20
makeSemanticValues [| AcquisitionVerb |] [GET]

acquisitionVerbs :: HashSet AcquisitionVerb
acquisitionVerbs = fromList [get]  -- 'get' generated by template
```

This generates: `get :: AcquisitionVerb` where `get = AcquisitionVerb GET`, providing a clean mapping from domain vocabulary to semantic types.

## Grammar Rules: Parsing Domain Commands

The parsing rules use Earley parsing to recognize valid domain command patterns:

```haskell  
-- From Grammar/Parser/Rules/Composites/Verbs.hs:112-128
acquisitionVerbPhraseRules :: Grammar r (Prod r Text Lexeme AcquisitionVerbPhrase)
acquisitionVerbPhraseRules = do
  determiner <- parseRule determiners Determiner
  adj <- parseRule adjectives Adjective  
  sourceMarker <- parseRule sourceMarkers SourceMarker
  acquisitionVerb <- parseRule acquisitionVerbs AcquisitionVerb
  object <- parseRule objectives Objective
  objectPhrase <- objectPhraseRules determiner adj object
  supportPhrase <- supportPhraseRules determiner adj
  rule $ SimpleAcquisitionVerbPhrase 
           <$> acquisitionVerb 
           <*> objectPhrase
         <|> AcquisitionVerbPhrase
           <$> acquisitionVerb
           <*> objectPhrase  
           <*> sourceMarker
           <*> supportPhrase
```

This implements the domain's **acquisition case frame**: valid patterns for taking objects in the game world.

## Domain Command Language

All case structures combine into the complete domain command language:

```haskell
-- From Model/Parser/Composites/Verbs.hs:94-102
data Imperative
  = Administrative AdministrativeVerb        -- "quit", "inventory"
  | ContainerAccessVerbPhrase' ContainerAccessVerbPhrase -- "open cabinet"  
  | StimulusVerbPhrase StimulusVerbPhrase    -- "look", "open eyes"
  | ConsumptionVerbPhrase' ConsumptionVerbPhrase -- "eat pill"
  | AcquisitionVerbPhrase' AcquisitionVerbPhrase -- "get robe"
  | PosturalVerbPhrase PosturalVerbPhrase    -- "stand up", "sit down"
```

This defines **the complete grammar of Sasha's domain-specific command language** - every valid command the system can understand and execute.

## Case Grammar Benefits for Domain-Specific Commands

1. **Domain Structure**: Commands are organized around game actions and the entities that participate in those actions

2. **Systematic Composition**: Complex commands build from well-defined semantic relationships within the domain

3. **Type Safety**: Invalid combinations rejected at parse time (can't eat a chair, can't open a pill)

4. **Extensibility**: New case frames add new command patterns without affecting existing ones

5. **Action Mapping**: Case structures map directly to game action dispatch:
   ```haskell
   AcquisitionVerbPhrase → AVManagementKey verb (GID AcquisitionActionF)
   ```

## Integration with Sasha's Action System

The parsed case structures generate `ActionManagement` keys for Sasha's dispatch system:

```haskell
-- From Model/Core.hs:481-494
data ActionManagement
  = AVManagementKey AcquisitionVerb (GID AcquisitionActionF)
  | SSAManagementKey SomaticAccessVerb (GID SomaticAccessActionF)  
  | CAManagementKey ConsumptionVerb (GID ConsumptionActionF)
  -- ...
```

The parser's job is to transform domain commands into the right action keys - the dispatch system handles execution.

## Summary

Sasha's parser demonstrates how case grammar provides a systematic foundation for designing domain-specific command languages for interactive fiction. Case grammar principles ensure this domain language is well-structured: organized around actions (verbs) and the game entities (nouns) that participate in those actions in specific semantic roles. The result is a parser that understands exactly the commands needed for the game domain, providing type-safe transformation from player input to actionable game commands.