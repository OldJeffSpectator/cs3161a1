module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP

type VEnv = E.Env Value

data VFun = VFun (Value -> Value)

instance Show VFun where
  show _ = error "Tried to show lambda"

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           | Lam VFun
           deriving (Show)

instance PP.Pretty Value where
  pretty (I i) = numeric i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined -- should not ever be used

evaluate :: Program -> Value
evaluate bs = evalE E.empty (Let bs (Var "main"))

lam :: (Value -> Value) -> Value
lam f = Lam $ VFun f

evalE :: VEnv -> Exp -> Value
evalE env (Var name) = case E.lookup env name of
  Just v -> v
  _      -> error $ "Not in scope: " ++ name
evalE _   (Prim op) = evalOp op
evalE _   (Con val) = case val of
  "True" -> B True
  "False" -> B False
  "Nil" -> Nil
  "Cons" -> lam $ \(I h) -> lam $ \t -> Cons h t
evalE _   (Num n) = I n
evalE env (If p t e) = case evalE env p of
  B True -> evalE env t
  B False -> evalE env e
evalE env (App e1 e2) = 
  let
    Lam (VFun f) = evalE env e1
    x = evalE env e2
  in f x
evalE env (Let [] body) = evalE env body
evalE env (Let (bind : binds) body) = case bind of
  Bind name _ args def -> 
    let env' = E.add env (name, bindLam env args def)
    in evalE env' (Let binds body)
evalE env f@(Letfun (Bind name _ args body)) =
  let env' = E.add env (name, evalE env f)
  in bindLam env' args body
evalE env (Letrec binds body) = 
  let
    eval (Bind name _ args def) = (name, bindLam env' args def)
    bindVals = eval `map` binds
    env' = E.addAll env bindVals
  in evalE env' body

bindLam :: VEnv -> [Id] -> Exp -> Value
bindLam env [] body = evalE env body
bindLam env (arg : args) body = 
  lam $ \v ->
    let env' = E.add env (arg, v)
    in bindLam env' args body

evalOp :: Op -> Value
evalOp op = 
  let 
    intOp f = lam $ \(I n1) -> lam $ \(I n2) -> I (n1 `f` n2)
    boolOp f = lam $ \(I n1) -> lam $ \(I n2) -> B (n1 `f` n2)
  in case op of
    Add  -> intOp (+)
    Sub  -> intOp (-)
    Mul  -> intOp (*)
    Quot -> intOp div
    Rem  -> intOp rem
    Eq   -> boolOp (==)
    Gt   -> boolOp (>)
    Ge   -> boolOp (>=)
    Lt   -> boolOp (<)
    Le   -> boolOp (<=)
    Ne   -> boolOp (/=)
    Neg  -> lam $ \(I n) -> I (-n)
    Null -> lam $ \l -> case l of
      Cons _ _ -> B False
      Nil      -> B True
    Head -> lam $ \l -> case l of
      Cons i _ -> I i
      _        -> error "Head of Nil!"
    Tail -> lam $ \l -> case l of
      Cons _ t -> t
      _        -> error "Tail of Nil!"
