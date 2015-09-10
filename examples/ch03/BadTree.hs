module BadTree where

import Tree

{-- snippet bad_nodesAreSame --}
bad_nodesAreSame (Node a _ _) (Node a _ _) = Just a
bad_nodesAreSame _            _            = Nothing
{-- /snippet bad_nodesAreSame --}

{-- snippet nodesAreSame --}
nodesAreSame (Node a _ _) (Node b _ _)
    | a == b     = Just a
nodesAreSame _ _ = Nothing
{-- /snippet nodesAreSame --}
