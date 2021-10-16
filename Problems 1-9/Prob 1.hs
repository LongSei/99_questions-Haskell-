myLast :: [a] -> a 
myLast [] = error "Empty"
myLast [x] = x
myLast (_:xs) = myLast xs
