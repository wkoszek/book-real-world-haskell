{-- snippet main --}
-- lines beginning with "--" are comments.

main = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"
{-- /snippet main --}
  
