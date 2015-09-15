import JSONClass

result :: JValue
result = toJValue (jobject [
                    ("query", toJValue "awkward squad haskell"),
                    ("estimatedCount", toJValue (3920::Int)),
                    ("moreResults", toJValue True)
                   ])

{-
result = jobject [
          ("query", "awkward squad haskell"),
          ("estimatedCount", 3920),
          ("moreResults", True),
          ("results", JArray [
                      JObject [
                       ("title", JString "Simon Peyton Jones: papers"),
                       ("snippet", JString "Tackling the awkward ..."),
                       ("url", JString "http://.../marktoberdorf/")
                      ]])
         ]
-}
