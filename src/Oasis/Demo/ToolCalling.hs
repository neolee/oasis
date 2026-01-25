module Oasis.Demo.ToolCalling
  ( toolCallingSystemMessage
  , toolCallingUserMessage
  , toolCallingMessages
  , toolCallingTools
  ) where

import Relude
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Oasis.Chat.Message (systemMessage, userMessage)
import Oasis.Types (Message, Tool(..), ToolFunctionSpec(..))

toolCallingSystemMessage :: Text
toolCallingSystemMessage = T.unlines
  [ "你是一个很有帮助的助手。"
  , "如果用户提问关于天气的问题，请调用 ‘get_current_weather’ 函数；"
  , "如果用户提问关于时间的问题，请调用 ‘get_current_time’ 函数。"
  , "请以友好的语气回答问题。"
  ]

toolCallingUserMessage :: Text
toolCallingUserMessage = "上海天气"

toolCallingMessages :: [Message]
toolCallingMessages =
  [ systemMessage toolCallingSystemMessage
  , userMessage toolCallingUserMessage
  ]

toolCallingTools :: [Tool]
toolCallingTools =
  [ Tool
      { type_ = "function"
      , function = ToolFunctionSpec
          { name = "get_current_time"
          , description = Just "当你想知道现在的时间时非常有用。"
          , parameters = Aeson.object
              [ "type" .= ("object" :: Text)
              , "properties" .= Aeson.object []
              , "required" .= ([] :: [Text])
              ]
          }
      }
  , Tool
      { type_ = "function"
      , function = ToolFunctionSpec
          { name = "get_current_weather"
          , description = Just "当你想查询指定城市的天气时非常有用。"
          , parameters = Aeson.object
              [ "type" .= ("object" :: Text)
              , "properties" .= Aeson.object
                  [ "location" .= Aeson.object
                      [ "type" .= ("string" :: Text)
                      , "description" .= ("城市或县区，比如北京市、杭州市、余杭区等。" :: Text)
                      ]
                  ]
              , "required" .= (["location"] :: [Text])
              ]
          }
      }
  ]
