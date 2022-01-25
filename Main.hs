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

main = do 
  putStrLn "Test the function myIf:"  
  print(myIf False 4 5)    --calling a function 
  print(fact(5))
  print (roots(1,-8,6))
  print(factRec 5)
  print(estVide testLst1)