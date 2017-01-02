import Src.Model.Employee
import qualified Src.Model.SecureEmployee as SE
import Src.Model.Credential
import Src.IO.JSONHandler
import Src.IO.TestIOFunctions
import Security.SecureFlow
import Security.ThreeLevels
import Security.Unsecure
import Data.List

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

hatchSalary :: Int -> Hatch High Int Int
hatchSalary i = pure (\s -> Just $ s+i)

incSalary :: Int -> String -> [SE.SEmployee] -> [SE.SEmployee]
incSalary _ _ []       =  []
incSalary i t (se:ses) =  if t == ((\(Just a) -> a) $ open medium $ SE.email se)
                          then (SE.SEmployee f l b s e le):ses
                          else incSalary i t ses
                          where f = SE.firstName se
                                l = SE.lastName se
                                b = SE.birthdate se
                                s = (declassifyWith (hatchSalary i) (SE.salary se)) :: SecureFlow High Int
                                e = SE.email se
                                le = SE.leader se

showSalary :: Hatch High Int Int
showSalary = pure (\s -> Just s)

showEmployeeSalary :: String -> [SE.SEmployee] -> IO (Maybe Int)
showEmployeeSalary _ []       = return Nothing
showEmployeeSalary n (se:ses) = do  if n == ((\(Just a) -> a) $ open medium $ SE.email se)
                                    then return $ open low $ ((declassifyWith showSalary (SE.salary se)) :: SecureFlow Low Int)
                                    else showEmployeeSalary n ses

menu :: [SE.SEmployee] -> IO ()
menu se = do  putStr $ "0) Exit \n"
              putStr $ "1) See employees' public details \n"
              putStr $ "2) Increase an employee's salary \n"
              putStr $ "What do you want to do? "
              c <- getLine
              case c of "0" ->  return ()
                        "1" ->  do  putStr $ intercalate "\n\n" $ map (SE.viewPublicDetails medium) se
                                    putStr "\n\n"
                                    menu se
                        "2" ->  do  putStr $ "Enter the employee's email address: "
                                    t <- getLine
                                    putStr $ "Enter the increase: "
                                    i <- getNat
                                    case validate i of  Left vi ->  do  let se' = incSalary vi t se
                                                                        s <- showEmployeeSalary t se'
                                                                        putStr $ "The new salary is " ++ (show s)
                                                                        menu se'
                                                        Right e   -> do print e
                                                                        menu se


main :: IO ()
main = do {-cs <-getCredentials
          e <- askForLogin cs-}
          se <- getSecureEmployees
          --putStr $ "Welcome, " ++ e ++ "\n"
          menu se
