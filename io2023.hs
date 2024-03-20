{-# LANGUAGE BlockArguments #-}
import System.IO

gra :: String -> Int -> Int -> IO()
gra [] _ _= return ()
gra (x:xs) compScore humanScore = do
            putStrLn "Prosze podac znak odpowiadajacy papierowi kamieniowi lub nozycom:"
            s <- getLine
            let c = read s :: Char
            print x
            print (kamienPapier x c compScore humanScore)
            gra xs (fst (kamienPapier x c compScore humanScore)) (snd (kamienPapier x c compScore humanScore))



kamienPapier :: Char -> Char -> Int -> Int -> (Int, Int)
kamienPapier 'P' 'K' cS hS = (cS+1, hS)
kamienPapier 'P' 'N' cS hS = (cS, hS+1)
kamienPapier 'K' 'N' cS hS = (cS+1, hS)
kamienPapier 'K' 'P' cS hS = (cS, hS+1)
kamienPapier 'N' 'P' cS hS = (cS+1, hS)
kamienPapier 'N' 'K' cS hS = (cS, hS+1)
kamienPapier _ _ cS hS = (cS, hS)



main :: IO ()
main = do
    gra "PKNP" 0 0
    s <- getLine
    return ()