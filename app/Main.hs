import Src.Model.Employee
import Src.Model.Credential
import Src.IO.JSONHandler
import Security.SecureFlow
import Security.ThreeLevels
import Security.Unsecure

login :: String -> String -> Hatch High [Credential] Bool
login e p = pure (\cs -> Just $ elem (Credential e p) cs)

askForLogin :: SecureFlow High [Credential] -> IO String
askForLogin cs = do putStr "Email: "
                    e <- getLine
                    putStr "Password: "
                    p <- getLine
                    let check = (declassifyWith (login e p) cs) :: SecureFlow Medium Bool
                        success = open medium check
                    case success of Just True  -> return e
                                    _          -> do  putStr "Incorrect credentials, please try again\n"
                                                      askForLogin cs

main :: IO ()
main = do cs <-getCredentials
          e <- askForLogin cs
          putStr $ "Welcome, " ++ e ++ "\n"
          putStr $ "What do you want to do? \n"
          putStr $ "1) See employees \n"
          c <- getLine
          case c of "1" -> do ue <- getEmployees
                              case validate ue of
                                Left es     -> print es
                                Right [err] -> print $ "There are some inconsistences " ++ (show err)
