{-- snippet notQuiteRight --}
import Control.Concurrent

notQuiteRight = do
  mv <- newEmptyMVar
  forkIO $ expensiveComputation_stricter mv
  someOtherActivity
  result <- takeMVar mv
  print result
{-- /snippet notQuiteRight --}

{-- snippet expensiveComputation --}
expensiveComputation mv = do
  let a = "this is "
      b = "not really "
      c = "all that expensive"
  putMVar mv (a ++ b ++ c)
{-- /snippet expensiveComputation --}

someOtherActivity = return ()
