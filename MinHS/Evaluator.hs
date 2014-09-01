module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP

type VEnv = E.Env Value

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           -- Others as needed
           deriving (Show)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined -- should not ever be used

evaluate :: Program -> Value
evaluate bs = evalE E.empty (Let bs (Var "main"))


evalE :: VEnv -> Exp -> Value
evalE g (Var i) = case E.lookup g i of
  Just v  ->  v
  Nothing ->  error $ "Variable " ++ show i ++ " not in scope"
evalE _ (Num n) = I n
evalE _ (Con i) = case i of
  "true" -> B True
  "false" -> B False
evalE g (If p t e) = case evalE g p of
  B True -> evalE g t
  B False -> evalE g e
  _ -> error $ "Expression " ++ show p ++ " did not eval to a bool"