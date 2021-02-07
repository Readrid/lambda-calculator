# Lambda Calculator

* Следующий тип данных используется для описания термов чистого нетипизированного лямбда-иcчиcления:
  ```Haskell
  type Symb = String 

  infixl 2 :@

  data Expr = Var Symb
            | Expr :@ Expr
            | Lam Symb Expr
            deriving (Eq)
  ```
  
* Реализован парсер лямбда термов при помощи библиотеки ```parsec```. 
  Комбинатор ```Haskell Lam "x" $ (Var "x") :@ (Var "y")``` представляется в виде ```\x -> x y```
  

* Алгоритм подстановки терма ```n``` вместо всех свободных вхождений переменной ```v``` в терме ```m```.
  ```Haskell
  subst :: Symb -> Expr -> Expr -> Expr 
  subst v n m = ...
  ```
* Алгоритм проверки альфа-эквивалентности двух термов
  ```Haskell
  alphaEq :: Expr -> Expr -> Bool
  alphaEq = ...
  ```
* Алгоритм многошаговой бета-редукции, использующий нормальную стратегию
  ```Haskell
  nf :: Expr -> Expr 
  nf = ...
  ```
* Алгоритм проверки бета-эквивалентности двух термов
  ```Haskell
  betaEq :: Expr -> Expr -> Bool 
  betaEq = ...
  ```
