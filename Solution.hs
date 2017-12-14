module Solution where

import Types

type StrType = [(String, Type)]

strtype_ :: (Symbol, Type) -> StrType -> StrType
strtype_ x strtype = x : strtype

typepool :: Symbol -> StrType -> Either String Type
typepool x strtype = case lookup x strtype of
  Just t -> Right t
  Nothing -> Left $ "znachenie " ++ show x ++ " ne naideno"


typeOf :: Term -> Either String Type
typeOf t = typeOff [] t

typeOff :: StrType -> Term -> Either String Type
typeOff strtype (Sym x) = typepool x strtype 

typeOff strtype (Lam x type1 t1) = case typeOff (strtype_ (x, type1) strtype) t1 of
  Right type2 -> Right (Fun type1 type2)
  _           -> Left $ "nesootvetstvie tipov"

typeOff strtype (App t1 t2) =
  case typeOff strtype t1 of
    Right (Fun r1 r2) ->  
      case typeOff strtype t2 of
        Right r3 ->
          if r1 == r3
            then Right r2
          else
            Left "Nesootvetstvie argumenta 1 and 2"
    Right _ -> Left "1 Argument ne Fun(lam) v (App)"
    Left e -> Left e

typeOff strtype (Boolean _) = Right Bool

typeOff strtype (Not t) = 
  case typeOff strtype t of 
    Right Bool -> Right Bool
    Right _ -> Left "Znachenie ne Bool"
    Left s -> Left s

typeOff strtype (And t1 t2) = 
  case typeOff strtype t1 of 
    Right Bool -> 
      case typeOff strtype t2 of
        Right Bool -> Right Bool 
        Right _ -> Left "2 Argument (And) ne Bool"
        Left e -> Left e
    Right _ -> Left "1 Argument (And) ne Bool"
    Left e -> Left e

typeOff strtype (Or t1 t2) = 
  case typeOff strtype t1 of
    Right Bool ->
      case typeOff strtype t2 of
        Right Bool -> Right Bool 
        Right _ -> Left "2 Argument (Or) ne Bool"
        Left e -> Left e
    Right _ -> Left "1 Argument (Or) ne Bool"
    Left e -> Left e

typeOff strtype (Iff t1 t2 t3) = 
  case typeOff strtype t1 of
    Right Bool ->
      if ((typeOff strtype t2) == (typeOff strtype t3))
        then typeOff strtype t2
      else
        Left "2 and 3 Argumenty raznye"
    Right _ -> Left "1 Argument (If) ne Bool"
    Left e -> Left e

typeOff strtype (Add t1 t2) = 
  case typeOff strtype t1 of 
    Right Nat -> 
      case typeOff strtype t2 of
        Right Nat -> Right Nat
        Right _ -> Left "2 Argument (Add) ne Nat"
        Left e -> Left e
    Right _ -> Left "1 Argument (Add) ne Nat"
    Left e -> Left e

typeOff strtype (Mult t1 t2) = 
  case typeOff strtype t1 of 
    Right Nat -> 
      case typeOff strtype t2 of
        Right Nat -> Right Nat
        Right _ -> Left "2 Argument (Mult) ne Nat"
        Left e -> Left e
    Right _ -> Left "1 Argument (Mult) ne Nat"
    Left e -> Left e

typeOff strtype (Pair t1 t2) = 
  case typeOff strtype t1 of
    Right t1 -> 
      case typeOff strtype t2 of
        Right t2 -> Right (PairT t1 t2)
        Left e -> Left e
    Left e -> Left e

typeOff strtype (Fst t) = 
  case typeOff strtype t of
    Right (PairT t1 t2) -> Right t1
    Right _ -> Left "Argument ne PairT (Fst)"
    Left e -> Left e

typeOff strtype (Snd t) = 
  case typeOff strtype t of
    Right (PairT t1 t2) -> Right t2
    Right _ -> Left "Argument ne PairT (Snd)"
    Left e -> Left e

typeOff strtype (Nil t) = Right (List t)

typeOff strtype (Cons t1 t2) = 
  case typeOff strtype t2 of
    Right (List t3) ->
      if((typeOff strtype t1) ==  (Right t3))
        then (Right (List t3))
      else
        Left "Argument 1 ne covpodaet c tipom spiska (Cons)"
    Right _ -> Left "Argument 2 ne List"
    Left e -> Left e

typeOff strtype (IsNil t) = 
  case typeOff strtype t of
    Right (List _) -> Right Bool
    Right _ -> Left "Argument ne List (isNil)"
    Left e -> Left e

typeOff strtype (Head t) = 
  case typeOff strtype t of
    Right (List t1) -> Right t1
    Right _ -> Left "Argument ne List (Head)"
    Left e -> Left e

typeOff strtype (Tail t) = 
  case typeOff strtype t of
    Right (List t1) -> Right t1
    Right _ -> Left "Argument ne List (Tail)"
    Left e -> Left e

typeOff strtype (Natural _) = Right Nat

-- > typeOf $ Lam "x" Nat $ Add (Sym "x") (Natural 5)
-- Right (Fun Nat Nat)

-- > typeOf $ Lam "x" Bool $ Sym "x"
-- Right (Fun Bool Bool)

-- > typeOf $ Add (Natural 5) (Boolean False)
-- Left "..."

-- > typeOf $ App (Lam "x" Nat $ Sym "x") (Natural 5)
-- Right Nat

-- > typeOf $ App (Lam "x" Nat $ Boolean False) (Natural 5)
-- Right Bool

-- > typeOf $ App (Lam "x" Bool $ Boolean False) (Natural 5)
-- Left "..."

-- > typeOf $ Nil Nat
-- Right (List Nat)

-- > typeOf $ Cons (Natural 5) $ Cons (Boolean False) $ Nil Nat
-- Left "..."
