{-- snippet module --}
module SimpleJSON
    (
      JValue(..)
    , getString
    , getInt
    , getDouble
    , getBool
    , getObject
    , getArray
    , isNull
    ) where
{-- /snippet module --}

{-- snippet JValue --}
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)
{-- /snippet JValue --}

{-- snippet getString --}
getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing
{-- /snippet getString --}

{-- snippet getters --}
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool (JBool b) = Just b
getBool _         = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray a) = Just a
getArray _          = Nothing

isNull v            = v == JNull
{-- /snippet getters --}
