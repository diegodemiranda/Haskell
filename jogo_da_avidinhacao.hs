{- Bibliotecas e parâmetros necessários para a execução -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Random ( randomRIO )


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Seja bem-vindo ao Jogo de Adivinhação!"
  startGame

{- Função principal -}
startGame :: IO ()
startGame = do
  randomNumber <- randomRIO (1, 100) :: IO Int
  putStrLn "Está preparado?\n Estou pensando em um número entre 1 e 100. Tente adivinhá-lo!"
  mechanics randomNumber 1

filepath :: String
filepath = "higherscore.txt"

{- Função com a mecânica do jogo -}
mechanics  :: Int -> Int -> IO ()
mechanics randomNumber attempts = do
  putStrLn ("• Tentativa " ++ show attempts ++ ":")
  putStr "• Palpite: "
  guess <- readLn :: IO Int
  case compare guess randomNumber of
    LT -> do
      putStrLn ("O número " ++ show guess ++ " do teu palpite é menor do que o número correto.\n") 
      mechanics randomNumber (attempts + 1)
    GT -> do
      putStrLn ("O número " ++ show guess ++ " do teu palpite é maior do que o número correto.\n") 
      mechanics randomNumber (attempts + 1)
    EQ -> do
      putStrLn "Parabéns! Você acertou."
      putStrLn ("O número em que eu estava pensando era " ++ show guess)
      putStrLn ("• Quantidade de palpites dados: " ++ show attempts)
      higherscore attempts
      tryAgain

{- Função que registra os recordes e atualiza o arquivo -}
higherscore :: Int -> IO ()
higherscore newScore = do
  contents <- readFile filepath
  let currentHigherScore = read contents
  if newScore < currentHigherScore || currentHigherScore == 0
    then do
      putStrLn "Você alcançou um novo recorde!\n"
      writeFile filepath (show newScore)
    else return ()

{- Função de encerramento ou continuação do jogo -}
tryAgain :: IO ()
tryAgain = do
  putStr "Você deseja jogar novamente?\n(s) para 'sim' ou (n) para 'não': " 
  choice <- getLine
  if choice == "s"
    then startGame
    else putStrLn "Espero que tenha se divertido.\nObrigado!"



