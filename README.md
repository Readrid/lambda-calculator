# Lambda Calculator

* Алгоритм подстановки терма ```n``` вместо всех свободных вхождений переменной ```v``` в терме ```m```.
  ```Haskell
  subst :: Symb -> Expr -> Expr -> Expr 
  subst v n m
  ```
