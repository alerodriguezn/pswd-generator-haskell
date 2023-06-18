import System.Random
import Data.List

-- Function to generate a random password of given length
generatePassword :: Int -> IO String
generatePassword len = do
  gen <- newStdGen
  let charSet = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] -- Character set to choose from
      password = take len (randomRs (0, length charSet - 1) gen) -- Generate random indices
  return $ map (charSet !!) password -- Map the indices to characters from the character set

-- Main function to interact with the user
main :: IO ()
main = do
  putStrLn "Enter the length of the password: "
  lenStr <- getLine
  let len = read lenStr :: Int
  password <- generatePassword len
  putStrLn $ "Generated password: " ++ password