myIf cond ifTrue ifFalse 
  | cond == True = ifTrue 
  | cond == False = ifFalse

fact x 
  | x == 0 = 1
  | x /= 0 = x * fact(x-1)

roots :: (Float, Float, Float) -> (Float, Float) 
roots (a,b,c) = (x1, x2) where
  x1 = e + sqrt d / (2*a)
  x2 = e - sqrt d / (2*a)
  d = b*b - 4*a*c
  e = -b/(2*a)

factRec 0 = 1
factRec n = n * factRec(n-1)

estVide lst = case lst of [] -> True; (_:_) -> False


testLst = [5,8,9,4]
testLst1 = []

msg = "aaaabccaaddddd"
--"aaaabccaaddddd"
--[(4,"a"),(1,"b"),(2,"c"),(2,"a"),(5,"d")]
encode [] = []
encode (x:xs) =  --take the first element of the list 
  let res = encode xs --temp variable
  in case res of
    [] -> [(1,x)]
    ((nb,elem):others) --do this action 
      | x == elem -> (nb+1,x):others  --if same elem -> do this
      | otherwise -> (1,x):res --if other case -> do that
  


main = do 
  putStrLn "Test the function myIf:"  
  print(myIf False 4 5)    --calling a function 
  print(fact(5))
  print (roots(1,-8,6))
  print(factRec 5)
  print(estVide testLst1)
  print(encode msg)