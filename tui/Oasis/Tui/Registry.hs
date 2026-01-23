module Oasis.Tui.Registry
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
      runChatInitAction,
      runHooksAction,
      runStructuredJsonAction,
      runStructuredSchemaAction )
import Oasis.Tui.Actions.Models
    ( runResponsesAction, runModelsAction, runEmbeddingsAction )
import Oasis.Tui.Actions.ToolCalling ( runToolCallingAction )
import Oasis.Tui.Actions.Completions
  ( runPartialModeAction, runPrefixCompletionAction, runFimCompletionAction )
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
  , RunnerSpec "chat" (NoPrompt runChatInitAction)
  , RunnerSpec "embeddings" (NeedsPrompt runEmbeddingsAction)
  , RunnerSpec "responses" (NeedsPrompt runResponsesAction)
  , RunnerSpec "models" (NoPrompt runModelsAction)
  , RunnerSpec "hooks" (NeedsPrompt runHooksAction)
  , RunnerSpec "structured-json" (NoPrompt runStructuredJsonAction)
  , RunnerSpec "structured-schema" (NoPrompt runStructuredSchemaAction)
  , RunnerSpec "tool-calling" (NoPrompt runToolCallingAction)
  , RunnerSpec "partial-mode" (NoPrompt runPartialModeAction)
  , RunnerSpec "prefix-completion" (NoPrompt runPrefixCompletionAction)
  , RunnerSpec "fim-completion" (NoPrompt runFimCompletionAction)
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
