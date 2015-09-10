import Control.Monad (ap, liftM, liftM3)

{-- snippet MovieReview --}
data MovieReview = MovieReview {
      revTitle :: String
    , revUser :: String
    , revReview :: String
    }
{-- /snippet MovieReview --}

{-- snippet simpleReview --}
simpleReview :: [(String, Maybe String)] -> Maybe MovieReview
simpleReview alist =
  case lookup "title" alist of
    Just (Just title@(_:_)) ->
      case lookup "user" alist of
        Just (Just user@(_:_)) ->
          case lookup "review" alist of
            Just (Just review@(_:_)) ->
                Just (MovieReview title user review)
            _ -> Nothing -- no review
        _ -> Nothing -- no user
    _ -> Nothing -- no title
{-- /snippet simpleReview --}

{-- snippet maybeReview --}
maybeReview alist = do
    title <- lookup1 "title" alist
    user <- lookup1 "user" alist
    review <- lookup1 "review" alist
    return (MovieReview title user review)

lookup1 key alist = case lookup key alist of
                      Just (Just s@(_:_)) -> Just s
                      _ -> Nothing
{-- /snippet maybeReview --}

{-- snippet liftedReview --}
liftedReview alist =
    liftM3 MovieReview (lookup1 "title" alist)
                       (lookup1 "user" alist)
                       (lookup1 "review" alist)
{-- /snippet liftedReview --}

{-- snippet apReview --}
apReview alist =
    MovieReview `liftM` lookup1 "title" alist
                   `ap` lookup1 "user" alist
                   `ap` lookup1 "review" alist
{-- /snippet apReview --}

attila =
{-- snippet attila --}
    [("name",       Just "Attila \"The Hun\""),
     ("occupation", Just "Khan")]
{-- /snippet attila --}
