{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module BrokenClass
    (
      JSON(..)
    , JValue(..)
    ) where

import SimpleJSON

{-- snippet class --}
type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a
{-- /snippet class --}

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue (JNumber v) = Right (round v)
    fromJValue _ = Left "not a JSON number"

instance JSON JValue where
    toJValue = id
    fromJValue = Right

{-- snippet string --}
instance JSON String where
    toJValue = JString
    fromJValue (JString s) = Right s
    fromJValue _ = Left "not a JSON string"
{-- /snippet string --}

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y -> Right (y:ys)
mapEithers _ _ = Right []

{-- snippet array --}
instance (JSON a) => JSON [a] where
    toJValue = undefined
    fromJValue = undefined
{-- /snippet array --}

{-- snippet object --}
instance (JSON a) => JSON [(String, a)] where
    toJValue = undefined
    fromJValue = undefined
{-- /snippet object --}
