seg :: String -> Char
seg [] = error "Informe uma string com pelo menos 2 caracteres"
seg (x:y:_) = y
seg _ = error "Informe uma string com pelo menos 2 caracteres"
