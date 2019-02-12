myLast :: [a] -> a
myLast []     = error "Error, List is empty!"
myLast [x]    = x
myLast (_:xs) = myLast xs


