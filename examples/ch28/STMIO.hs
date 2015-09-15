import Control.Concurrent.STM
import Control.Monad
import GHC.Conc

someAction = undefined

{-- snippet someTransaction --}
someAction :: IO a

stmTransaction :: STM (IO a)
stmTransaction = return someAction

doSomething :: IO a
doSomething = join (atomically stmTransaction)
{-- /snippet someTransaction --}

launchTorpedoes = undefined
doStuff = undefined
mightRetry = undefined
{-- snippet bad --}
launchTorpedoes :: IO ()

notActuallyAtomic = do
  doStuff
  unsafeIOToSTM launchTorpedoes
  mightRetry
{-- /snippet bad --}
