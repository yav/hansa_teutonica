module Common.Utils where

import Data.Text(Text)
import qualified Data.Text as Text
import Control.Monad.ST
import System.Random.TF
import System.Random.TF.Instances
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS

showText :: Show a => a -> Text
showText = Text.pack . show

enumAll :: (Bounded a,Enum a) => [a]
enumAll = [ minBound .. maxBound ]

shuffle :: [a] -> TFGen -> ([a],TFGen)
shuffle xs g0
  | null xs = ([],g0)
  | otherwise =
    runST do v <- Vector.unsafeThaw (Vector.fromList xs)
             mutShuffle v g0 (MVector.length v - 1)
  where
  mutShuffle v g i
    | i > 0 =
      case randomR (0,i) g of
        (j,g1) -> do swap v i j
                     mutShuffle v g1 (i-1)

    | otherwise = do v1 <- Vector.unsafeFreeze v
                     pure (Vector.toList v1,g)

  swap v a b =
    do x <- MVector.read v a
       y <- MVector.read v b
       MVector.write v a y
       MVector.write v b x

jsTag :: Text -> JS.Pair
jsTag t = "tag" JS..= t

jsTagged :: Text -> [JS.Pair] -> JS.Value
jsTagged t xs = JS.object (jsTag t : xs)

jsCall :: JS.ToJSON a => Text -> [a] -> JS.Value
jsCall f as = jsTagged f [ "args" JS..= as ]

jsCall' :: Text -> JS.Value
jsCall' f = jsTagged f [ "args" JS..= ([] :: [JS.Value]) ]

js :: JS.ToJSON a => a -> JS.Value
js = JS.toJSON

jsParseEnum ::
  (Bounded a, Enum a) => String -> (a -> Text) -> JS.Value -> JS.Parser a
jsParseEnum lab toTxt =
  JS.withText lab \txt ->
  case lookup txt [ (toTxt s, s) | s <- enumAll ] of
    Just a  -> pure a
    Nothing -> fail ("Invalid " ++ lab)


