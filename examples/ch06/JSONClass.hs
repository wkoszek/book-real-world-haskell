{-# LANGUAGE FlexibleInstances #-}
{-- snippet LANGUAGE --}
{-# LANGUAGE TypeSynonymInstances #-}
{-- /snippet LANGUAGE --}

{-- snippet module --}
module JSONClass
    (
      JSON(..)
    , JValue(..)
    , JAry(..)
    , JObj(..)
    ) where
{-- /snippet module --}

{-- snippet instance.JObj --}
import Control.Arrow (second)

instance (JSON a) => JSON (JObj a) where
    toJValue = JObject . JObj . map (second toJValue) . fromJObj

    fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
      where unwrap (k,v) = whenRight ((,) k) (fromJValue v)
    fromJValue _ = Left "not a JSON object"
{-- /snippet instance.JObj --}

{-- snippet JAry --}
newtype JAry a = JAry {
      fromJAry :: [a]
    } deriving (Eq, Ord, Show)
{-- /snippet JAry --}

{-- snippet jary --}
jary :: [a] -> JAry a
jary = JAry
{-- /snippet jary --}

{-- snippet JObj --}
newtype JObj a = JObj {
      fromJObj :: [(String, a)]
    } deriving (Eq, Ord, Show)
{-- /snippet JObj --}

{-- snippet JValue --}
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)   -- was [(String, JValue)]
            | JArray (JAry JValue)    -- was [JValue]
              deriving (Eq, Ord, Show)
{-- /snippet JValue --}

{-- snippet class --}
type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right
{-- /snippet class --}

{-- snippet String --}
instance JSON String where
    toJValue               = JString

    fromJValue (JString s) = Right s
    fromJValue _           = Left "not a JSON string"
{-- /snippet String --}

{-- snippet doubleToJValue --}
doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "not a JSON number"

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue = JNumber
    fromJValue = doubleToJValue id
{-- /snippet doubleToJValue --}

{-- snippet mapEithers --}
mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y -> Right (y:ys)
mapEithers _ _ = Right []
{-- /snippet mapEithers --}

{-- snippet listToJValues --}
listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue
{-- /snippet listToJValues --}

{-- snippet jvaluesToJAry --}
jvaluesToJAry :: [JValue] -> JAry JValue
jvaluesToJAry = JAry
{-- /snippet jvaluesToJAry --}

{-- snippet jaryOfJValuesToJValue --}
jaryOfJValuesToJValue :: JAry JValue -> JValue
jaryOfJValuesToJValue = JArray
{-- /snippet jaryOfJValuesToJValue --}

{-- snippet jaryToJValue --}
jaryToJValue = JArray . JAry . map toJValue . fromJAry
{-- /snippet jaryToJValue --}

{-- snippet jaryFromJValue --}
jaryFromJValue (JArray (JAry a)) =
    whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _ = Left "not a JSON array"
{-- /snippet jaryFromJValue --}

{-- snippet instance.JAry --}
jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)

jaryToJValue :: (JSON a) => JAry a -> JValue

instance (JSON a) => JSON (JAry a) where
    toJValue = jaryToJValue
    fromJValue = jaryFromJValue
{-- /snippet instance.JAry --}

{-- snippet Bool --}
instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"
{-- /snippet Bool --}

{-- snippet whenRight --}
whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)
{-- /snippet whenRight --}

instance (JSON a) => JSON (Maybe a) where
    toJValue = maybe JNull toJValue
    fromJValue JNull = Right Nothing
    fromJValue v = whenRight Just (fromJValue v)

class Wrapper w where
    rewrap :: (a -> b) -> w a -> w b

instance Wrapper JAry where
    rewrap f = JAry . map f . fromJAry

instance Wrapper JObj where
    rewrap f = JObj . map (second f) . fromJObj
