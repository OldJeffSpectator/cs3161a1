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
evalE env (Var id) = case E.lookup env id of
  Just v -> v
  Nothing -> error $ "Variable " ++ id ++ " not in scope."

evalE _ (Con id) = case id of
  "True" -> B True
  "False" -> B False
  _ -> error $ "Unknown constant \"" ++ id ++ "\"."

evalE _ (Num n) = I n

evalE env (If p t e) = case evalE env p of
  B True -> evalE env t
  B False -> evalE env e
  _ -> error $ "Expression " 
    ++ show p ++ " does not eval to a bool."

evalE env (Let [bind] body) = case bind of
  Bind id _ _ def -> 
    let env' = E.add env (id, evalE env def) in
      evalE env' body

evalE _ e = 
  let msg = PP.renderPretty 1.0 80 $ PP.pretty e in
    error $  "Unimplemented for:\n" ++
      PP.displayS msg ""