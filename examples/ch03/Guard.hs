{-- snippet fromMaybe --}
fromMaybe defval wrapped =
    case wrapped of
      Nothing     -> defval
      Just value  -> value
{-- /snippet fromMaybe --}
