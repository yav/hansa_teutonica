{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}
module Main where

import Data.Text(Text)
import qualified Data.Text.IO as Text
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Vector as Vector
import Data.Aeson as JS
import Data.Aeson(toJSON)
import Control.Monad(unless,guard)
import Control.Monad(fail)
import System.Environment(getArgs)

main :: IO ()
main =
  do args <- getArgs
     case args of
       [] ->
         do bs <- BS.getContents
            case JS.eitherDecode bs of
              Left err -> fail err
              Right (Msg t) -> Text.putStrLn t

       [file, xmlfile] ->
          do txt <- Text.readFile file
             xmltxt <- Text.readFile xmlfile
             let msg = upload txt xmltxt
             BS.putStrLn (JS.encode msg)



data Msg = Msg Text

instance JS.FromJSON Msg where
  parseJSON =
    withObject "message" \o ->
      do m <- o .: "messageID"
         case m :: Int of
           1 ->
              do va <- o .: "scriptStates"
                 v <- withArray "states" pure va
                 unless (Vector.length v == 1) $ fail "Multiple scrips"
                 Msg <$> withObject "script" (.: "script") (v Vector.! 0)
           2 -> Msg <$> o .: "message"
           3 -> Msg <$> o .: "error"
           _ -> fail "Unknown message number"



upload :: Text -> Text -> JS.Value
upload script xml = JS.object

  [ ("messageID", int 1)
  , ("scriptStates",
      arr
      [ object [ ("name", "Global")
               , ("guid", "-1")
               , ("script", text script)
               , ("ui", text xml)
               ]
      ]
    )
  ]

--------------------------------------------------------------------------------

text :: Text -> JS.Value
text = toJSON

int :: Int -> JS.Value
int = toJSON

arr :: [JS.Value] -> Value
arr = toJSON
