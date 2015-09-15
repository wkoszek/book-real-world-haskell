{-- snippet all --}
returnTest :: IO ()
returnTest =
    do one <- return 1
       let two = 2
       putStrLn $ show (one + two)
{-- /snippet all --}
