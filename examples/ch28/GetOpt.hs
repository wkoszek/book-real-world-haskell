{-- snippet ArgDescr --}
data ArgDescr a = NoArg a
                | ReqArg (String -> a) String
                | OptArg (Maybe String -> a) String
{-- /snippet ArgDescr --}
