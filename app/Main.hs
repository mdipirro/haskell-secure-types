import Src.Model.Employee
import Src.Model.Store
import qualified Src.Model.SecureEmployee as SE
import Src.Model.Credential
import Src.IO.JSONHandler
import Src.IO.TestIOFunctions
import Security.SecureFlow
import qualified Security.SecureComputation as SC
import Security.ThreeLevels
import Security.Unsecure
import Data.List

login :: String -> String -> Hatch High [Credential] Bool
login e p = pure (\cs -> elem (Credential e p) cs)

askForLogin :: SecureFlow High [Credential] -> IO String
askForLogin cs = do putStr "Email: "
                    e <- getLine
                    putStr "Password: "
                    p <- getLine
                    let check = (declassifyWith (login e p) cs) :: SecureFlow Medium Bool
                        success = open medium check
                    case success of True  -> return e
                                    _     -> do putStr "Incorrect credentials, please try again\n"
                                                askForLogin cs

showSalary :: Hatch' High Medium Int Int
showSalary = makeHatch id

calculateNewSalaries :: [SE.SEmployee] -> SecureFlow High [Int]
calculateNewSalaries es = pure $ map getIncrement es
                          where getIncrement e =  if open low $ SE.leader e
                                                  then 100
                                                  else 50


showEmployeeSalary :: String -> [SE.SEmployee] -> IO Int
showEmployeeSalary _ []       = return 0
showEmployeeSalary n (se:ses) = do  if n == (open medium $ SE.email se)
                                    then return $ open medium $ ((declassifyWith' showSalary (SE.salary se)) :: SecureFlow Medium Int)
                                    else showEmployeeSalary n ses

incSalary :: Int -> String -> [SE.SEmployee] -> [SE.SEmployee]
incSalary _ _ []       =  []
incSalary i t (se:ses) =  if t == (open medium $ SE.email se)
                          then (SE.increaseSalary i se):ses
                          else incSalary i t ses

storesOperation :: Read a => SC.SecureComputation SC.P String -> String -> (a -> Store -> Store)
                    -> SC.SecureComputation SC.P [Store] -> SC.SecureComputation SC.P [Store]
storesOperation m n f sc =  SC.sapp (SC.spure $ (\ss -> op m n ss)) sc
                            where op i n (s:ss) = if n == productName s
                                                  then (f (read . SC.open $ m) s):ss
                                                  else s:(op m n ss)


menu :: [SE.SEmployee] -> SC.SecureComputation SC.P [Store] -> IO ()
menu se ss = do putStr $ "\n\n0) Exit \n"
                putStr $ "1) See employees' public details \n"
                putStr $ "2) Increase an employee's salary \n"
                putStr $ "3) See stores status \n"
                putStr $ "4) Increase price \n"
                putStr $ "5) Increase stocks \n"
                putStr $ "What do you want to do? "
                c <- getLine
                case c of "0" ->  return ()
                          "1" ->  do  putStr $ intercalate "\n\n" $ map (SE.viewPublicDetails medium) se
                                      putStr "\n\n"
                                      menu se ss
                          "2" ->  do  putStr $ "Enter the employee's email address: "
                                      t <- getLine
                                      putStr $ "Enter the increase: "
                                      i <- getNat
                                      case validate i of  Left vi ->  do  let se' = incSalary (read vi) t se
                                                                          s <- showEmployeeSalary t se'
                                                                          putStr $ "The new salary is " ++ (show s)
                                                                          menu se' ss
                                                          Right e   -> do print e
                                                                          menu se ss
                          "3" -> do print $ SC.open ss
                                    menu se ss
                          "4" -> do putStr $ "Enter the product name: "
                                    p <- getLine
                                    putStr $ "Enter the increase: "
                                    --i <- getUnpureNat
                                    --UNSAFE!!
                                    l <- getLine
                                    let i = SC.spure l :: SC.SecureComputation SC.P String
                                    let ss' = storesOperation i p (increasePrice) ss
                                    print $ SC.open ss'
                                    menu se ss'
                          "5" -> do putStr $ "Enter the product name: "
                                    p <- getLine
                                    putStr $ "Enter the increase: "
                                    --i <- getUnpureNat
                                    --UNSAFE!!
                                    l <- getLine
                                    let i = SC.spure l :: SC.SecureComputation SC.P String
                                    let ss' = storesOperation i p (increaseStocks) ss
                                    print $ SC.open ss'
                                    menu se ss'



main :: IO ()
main = do cs <-getCredentials
          e <- askForLogin cs
          se <- getSecureEmployees
          ss <- getStores
          putStr $ "Welcome, " ++ e ++ "\n"
          menu se ss
