module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP

type VEnv = E.Env Value

data VFun = VFun (Value -> Value)

instance Show VFun where
  show _ = error $ "Tried to show lambda"

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           -- Others as needed
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
  "Cons" -> 
    lam $ \(I head) ->
      lam $ \tail ->
        Cons head tail
  _ -> error $ "Unknown constant \"" ++ id ++ "\"."

evalE _ (Num n) = I n

evalE env (If p t e) = case evalE env p of
  B True -> evalE env t
  B False -> evalE env e
  _ -> error $ "Expression " 
    ++ show p ++ " does not eval to a bool."

evalE env (Prim op) = evalOp op

evalE env (App e1 e2) = 
  let 
    f = evalE env e1
    x = evalE env e2
  in evalApp f x

evalE env (Let binds body) = 
  evalLet env binds body

evalE env (Letfun (Bind name _ args body)) =
  evalLetFun env name args body

evalE env (Letrec binds body) = 
  evalLetRec env binds body

evalE _ e = 
  let msg = PP.renderPretty 1.0 80 $ PP.pretty e in
    error $  "Unimplemented for:\n" ++
      PP.displayS msg ""

lam :: (Value -> Value) -> Value
lam f = Lam $ VFun f

bindLam :: VEnv -> [Id] -> Exp -> Value
bindLam env [] body = evalE env body
bindLam env (arg : args) body = 
  lam $ \v ->
    let env' = E.add env (arg, v)
    in bindLam env' args body

evalLet :: VEnv -> [Bind] -> Exp -> Value
evalLet env [] body = evalE env body
evalLet env (bind : binds) body = case bind of
  Bind name _ args def ->
    let env' = E.add env (name, bindLam env args def)
    in evalLet env' binds body

evalLetFun :: VEnv -> Id -> [Id] -> Exp -> Value
evalLetFun env name args body =
  -- recursive call binds the knot,
  -- distinguishes letFun from let
  let env' = E.add env (name, evalLetFun env name args body)
  in bindLam env' args body

evalLetRec :: VEnv -> [Bind] -> Exp -> Value
evalLetRec env binds body =
  -- using mutual recursion to implement mutual recursion?
  -- cheaty! 
  let
    eval (Bind name _ args body) = (name, bindLam env' args body)
    bindVals = eval `map` binds
    env' = E.addAll env bindVals
  in evalE env' body

evalApp :: Value -> Value -> Value  
evalApp (Lam (VFun g)) x = g x
evalApp f _ = error $ "Not a valid app term " ++ show f

evalOp :: Op -> Value
evalOp op = 
  let 
    intOp f = 
      lam $ \(I n1) ->
        lam $ \(I n2) -> I (n1 `f` n2)
    boolOp f = 
      lam $ \(I n1) ->
        lam $ \(I n2) -> B (n1 `f` n2)
  in case op of
    Add -> intOp (+)
    Sub -> intOp (-)
    Mul -> intOp (*)
    Quot -> intOp div
    Rem -> intOp rem
    Eq -> boolOp (==)
    Gt -> boolOp (>)
    Ge -> boolOp (>=)
    Lt -> boolOp (<)
    Le -> boolOp (<=)
    Ne -> boolOp (/=)
    Neg -> lam $ \(I n) -> I (-n)
    Null -> lam $ \l -> case l of
      Nil -> B True
      Cons _ _ -> B False
    Head -> lam $ \l -> case l of
      Nil -> error $ "Head of nil!"
      Cons i _ -> I i
    Tail -> lam $ \l -> case l of
      Nil -> error $ "Tail of nil!"
      Cons _ t -> t
