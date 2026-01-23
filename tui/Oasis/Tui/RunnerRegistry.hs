module Oasis.Tui.RunnerRegistry
  ( RunnerAction(..)
  , RunnerSpec(..)
  , runnerRegistry
  , runnerNames
  , lookupRunner
  , runnerRequiresPrompt
  ) where

import Relude
import Brick.Types (EventM)
import Oasis.Tui.Actions.Chat
    ( runBasicAction,
      runHooksAction,
      runStructuredJsonAction,
      runStructuredSchemaAction )
import Oasis.Tui.Actions.Models
    ( runResponsesAction, runModelsAction, runEmbeddingsAction )
import Oasis.Tui.Actions.ToolCalling ( runToolCallingAction )
import Oasis.Tui.State (AppState, Name)


data RunnerAction
  = NeedsPrompt (Text -> EventM Name AppState ())
  | NoPrompt (EventM Name AppState ())
  | Unsupported


data RunnerSpec = RunnerSpec
  { runnerName :: Text
  , runnerAction :: RunnerAction
  }

runnerRegistry :: [RunnerSpec]
runnerRegistry =
  [ RunnerSpec "basic" (NeedsPrompt runBasicAction)
  , RunnerSpec "responses" (NeedsPrompt runResponsesAction)
  , RunnerSpec "embeddings" (NeedsPrompt runEmbeddingsAction)
  , RunnerSpec "hooks" (NeedsPrompt runHooksAction)
  , RunnerSpec "models" (NoPrompt runModelsAction)
  , RunnerSpec "structured-json" (NoPrompt runStructuredJsonAction)
  , RunnerSpec "structured-schema" (NoPrompt runStructuredSchemaAction)
  , RunnerSpec "tool-calling" (NoPrompt runToolCallingAction)
  , RunnerSpec "chat" Unsupported
  , RunnerSpec "partial-mode" Unsupported
  , RunnerSpec "prefix-completion" Unsupported
  , RunnerSpec "fim-completion" Unsupported
  ]

runnerNames :: [Text]
runnerNames = map runnerName runnerRegistry

lookupRunner :: Text -> Maybe RunnerSpec
lookupRunner name = find ((== name) . runnerName) runnerRegistry

runnerRequiresPrompt :: Maybe Text -> Bool
runnerRequiresPrompt mName =
  case mName >>= lookupRunner of
    Just RunnerSpec{runnerAction = NeedsPrompt _} -> True
    _ -> False
