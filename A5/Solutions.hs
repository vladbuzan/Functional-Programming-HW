module Main where
import Data.Char(isLower, isUpper, isDigit)
import Data.List

passwords :: [String]
passwords = 
    let
        list = ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']
    in
        sequenceA (replicate 8 list)

data User = User {email :: Email, password :: Password} deriving (Show, Eq)

data Password = Password String deriving (Show, Eq)

data Email = Email {username :: String, domain:: String} deriving (Show, Eq)

validatePassword :: String -> Maybe Password
validatePassword password 
    |((length password) >= 8) && (any isLower password) && (any isUpper password)
    && (any isDigit password) = Just (Password password)
    | otherwise = Nothing


validateEmail :: String -> Maybe Email
validateEmail email = 
    let
        noOfAts = length (elemIndices '@' email) 
        (username, domain) = span (/= '@') email
        lastPointIndex = if length (elemIndices '.' domain) > 0
            then last (elemIndices '.' domain)
            else -1
        (hostname, topLevelDomain) = splitAt lastPointIndex domain
    in
    if ((length username) < 3) 
        then Nothing
    else 
        if (noOfAts /= 1) 
            then Nothing
        else
            if (lastPointIndex < 1) 
                then Nothing
            else
                if (length hostname < 1) 
                    then Nothing
                else
                    Just (Email username (drop 1 domain))


validateUser :: String -> String -> Maybe User
validateUser email password = 
    let
        (vEmail, vPassword) = (validateEmail email, validatePassword password)
    in
    case (vEmail, vPassword) of 
    (Just x, Just y) -> Just (User x y)
    (_, _) -> Nothing


main :: IO ()
main = do
    putStrLn "Please enter you email address"
    email <- getLine
    putStrLn "Please enter you password"
    password <- getLine
    case validateUser email password of
        Just x -> putStrLn "Valid user"
        _ -> putStrLn "Invalid user"
    