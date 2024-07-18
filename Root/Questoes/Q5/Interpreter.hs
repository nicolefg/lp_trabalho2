module Root.Questoes.Q5.Interpreter where

import Root.Questoes.Q5.AbsLI
import Prelude hiding (lookup)

type ErrorMessage = String

executeP :: RContext -> Program -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm

data Valor
  = ValorStr String
  | ValorInt Integer
  | ValorBool Bool

instance Eq Valor where
  (ValorInt i1) == (ValorInt i2) = i1 == i2
  (ValorStr s1) == (ValorStr s2) = s1 == s2
  (ValorBool b1) == (ValorBool b2) = b1 == b2

instance Show Valor where
  show (ValorInt vint) = show vint
  show (ValorStr vstr) = vstr
  show (ValorBool vb) = show vb

s :: Valor -> String
s (ValorStr str) = str

i :: Valor -> Integer
i (ValorInt vint) = vint

b :: Valor -> Bool
b (ValorBool vbool) = vbool

addValor :: Valor -> Valor -> Either ErrorMessage Valor
addValor (ValorInt i1) (ValorInt i2) = Right (ValorInt (i1 + i2))
addValor _ _ = Left "Invalid types for addition"

subValor :: Valor -> Valor -> Either ErrorMessage Valor
subValor (ValorInt i1) (ValorInt i2) = Right (ValorInt (i1 - i2))
subValor _ _ = Left "Invalid types for subtraction"

mulValor :: Valor -> Valor -> Either ErrorMessage Valor
mulValor (ValorInt i1) (ValorInt i2) = Right (ValorInt (i1 * i2))
mulValor _ _ = Left "Invalid types for multiplication"

divValor :: Valor -> Valor -> Either ErrorMessage Valor
divValor (ValorInt _) (ValorInt 0) = Left "divisao por 0"
divValor (ValorInt i1) (ValorInt i2) = Right (ValorInt (i1 `div` i2))
divValor _ _ = Left "Invalid types for division"

execute :: RContext -> Stm -> Either ErrorMessage RContext
execute context x = case x of
  SAss id exp -> case eval context exp of
    Right v -> Right (update context (getStr id) v)
    Left msg -> Left msg
  SBlock [] -> Right context
  SBlock (s : stms) -> case execute context s of
    Right ctx -> execute ctx (SBlock stms)
    Left msg -> Left msg
  SWhile exp stm -> case eval context exp of
    Right (ValorInt n) ->
      if n /= 0
        then case execute context stm of
          Right ctx -> execute ctx (SWhile exp stm)
          Left msg -> Left msg
        else Right context
    _ -> Left "Expected integer value in while condition"
  SdoWhile stm exp -> case execute context stm of
    Right ctx -> case eval ctx exp of
      Right (ValorInt i) ->
        if i == 0
          then execute context stm
          else case execute context stm of
            Right ctx -> execute ctx (SdoWhile stm exp)
            Left msg -> Left msg
      _ -> Left "Expected integer value in do-while condition"
    Left msg -> Left msg
  STry (t : try) catch finally -> case execute context t of
    Right ctx -> execute ctx (STry try catch finally)
    Left msg -> case execute context (SBlock catch) of
      Right ctx -> execute ctx (SBlock finally)
      Left msg -> Left msg
  STry [] _ finally -> execute context (SBlock finally)

eval :: RContext -> Exp -> Either ErrorMessage Valor
eval context x = case x of
  EAdd exp0 exp -> do
    ve1 <- eval context exp0
    ve2 <- eval context exp
    addValor ve1 ve2
  ESub exp0 exp -> do
    ve1 <- eval context exp0
    ve2 <- eval context exp
    subValor ve1 ve2
  EMul exp0 exp -> do
    ve1 <- eval context exp0
    ve2 <- eval context exp
    mulValor ve1 ve2
  EDiv exp0 exp -> do
    ve1 <- eval context exp0
    ve2 <- eval context exp
    divValor ve1 ve2
  EInt n -> Right (ValorInt n)
  EVar id -> case contextLookup context (getStr id) of
    Just val -> Right val
    Nothing -> Left "Variable not found"
  EStr s -> Right (ValorStr s)
  ETrue -> Right (ValorBool True)
  EFalse -> Right (ValorBool False)
  EAnd exp0 exp -> do
    ve1 <- eval context exp0
    ve2 <- eval context exp
    case (ve1, ve2) of
      (ValorBool b1, ValorBool b2) -> Right (ValorBool (b1 && b2))
      _ -> Left "Expected boolean values in 'and' expression"
  EOr exp0 exp -> do
    ve1 <- eval context exp0
    ve2 <- eval context exp
    case (ve1, ve2) of
      (ValorBool b1, ValorBool b2) -> Right (ValorBool (b1 || b2))
      _ -> Left "Expected boolean values in 'or' expression"
  ENot exp -> do
    ve <- eval context exp
    case ve of
      ValorBool b -> Right (ValorBool (not b))
      _ -> Left "Expected boolean value in 'not' expression"
  ECon exp0 exp -> do
    ve1 <- eval context exp0
    ve2 <- eval context exp
    case (ve1, ve2) of
      (ValorStr s1, ValorStr s2) -> Right (ValorStr (s1 ++ s2))
      _ -> Left "Expected string values in 'concat' expression"

type RContext = [(String, Valor)]

getStr :: Ident -> String
getStr (Ident s) = s

contextLookup :: RContext -> String -> Maybe Valor
contextLookup [] _ = Nothing
contextLookup ((i, v) : cs) s
  | i == s = Just v
  | otherwise = contextLookup cs s

update :: RContext -> String -> Valor -> RContext
update [] s v = [(s, v)]
update ((i, v) : cs) s nv
  | i == s = (i, nv) : cs
  | otherwise = (i, v) : update cs s nv
