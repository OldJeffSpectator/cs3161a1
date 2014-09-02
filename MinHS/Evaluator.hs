module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP

type VEnv = E.Env Value

data VFun = VFun (Value -> Value)

instance Show VFun where
  show _ = "VFun ( ??? )"

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           -- Others as needed
           | Cloj VEnv Id [Id] Exp
           | Lam VFun
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
  "Nil" -> Nil
  _ -> error $ "Unknown constant \"" ++ id ++ "\"."
evalE _ (Num n) = I n
evalE env (If p t e) = case evalE env p of
  B True -> evalE env t
  B False -> evalE env e
  _ -> error $ "Expression " 
    ++ show p ++ " does not eval to a bool."
evalE env (App (App (Con "Cons") e1) e2) =
  let
    I head = evalE env e1
    tail = evalE env e2
  in Cons head tail
evalE env (App (Prim op) e) =
  let v = evalE env e 
  in evalUnaryOp op v
evalE env (App (App (Prim op) e1) e2) = 
  let
    v1 = evalE env e1
    v2 = evalE env e2
  in evalBinaryOp op v1 v2
evalE env (App e1 e2) = 
  let 
    f = evalE env e1
    x = evalE env e2
  in case f of
    Cloj fEnv name [arg] body ->
      let fEnv' = E.addAll fEnv [(name, f), (arg, x)]
      in evalE fEnv' body
    Lam (VFun g) -> g x
    _ -> error $ "Not a valid app term " ++ show e1
evalE env (Let binds body) = 
  evalLet env binds body
evalE env (Letfun (Bind name _ args body)) =
  evalLetFun env name args body
evalE _ e = 
  let msg = PP.renderPretty 1.0 80 $ PP.pretty e in
    error $  "Unimplemented for:\n" ++
      PP.displayS msg ""

evalLetFun :: VEnv -> Id -> [Id] -> Exp -> Value
evalLetFun env name [] body = 
  let env' = E.add env (name, evalLetFun env name [] body)
  in evalE env' body
evalLetFun env name args body =
  Cloj env name args body

evalUnaryOp :: Op -> Value -> Value
evalUnaryOp Neg (I n) = I (-n)
evalUnaryOp Null l = case l of
  Nil -> B True
  Cons _ _ -> B False
evalUnaryOp Head l = case l of
  Nil -> error $ "Head of nil"
  Cons i _ -> I i
  _ -> error $ "What? " ++ show l
evalUnaryOp Tail l = case l of
  Nil -> error $ "Tail of nil"
  Cons _ t -> t
evalUnaryOp op v = error $ "-- No unary op for op:\n"
  ++ show op
  ++ "\n-- value:\n"
  ++ show v

evalBinaryOp :: Op -> Value -> Value -> Value
evalBinaryOp op (I n1) (I n2) = case op of
  Add -> I (n1 + n2)
  Sub -> I (n1 - n2)
  Mul -> I (n1 * n2)
  Quot -> I (n1 `div` n2)
  Rem -> I (n1 `rem` n2)
  Eq -> B (n1 == n2)
  Gt -> B (n1 > n2)
  Ge -> B (n1 >= n2)
  Lt -> B (n1 < n2)
  Le -> B (n1 <= n2)
  Ne -> B (n1 /= n2)
  other -> error $ "No binary op for " ++ show other ++ "."
evalBinaryOp op l r = error $ "-- No binary op for op\n" 
  ++ show op
  ++ "\n-- left hand side\n"
  ++ show l
  ++ "\n-- right hand side\n"
  ++ show r

evalLet :: VEnv -> [Bind] -> Exp -> Value
evalLet _ [] _ = error $ "Empty bind list"
evalLet env (bind : binds) body = case bind of
  Bind id _ args def ->
    let env' = E.add env (id, evalE env def)
    in case binds of 
      [] -> evalE env' body
      _  -> evalLet env' binds body