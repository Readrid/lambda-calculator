# Lambda Calculator

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
