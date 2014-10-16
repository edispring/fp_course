module Firststeps where
    last xs = head (drop (length xs -1) xs)
    lastix xs = xs !! (length xs -1)
    init xs = reverse(tail(reverse xs))
    prod [] = 1
    prod (x:xs) = x * prod xs
    qsort [] = []
    qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
        where smaller = [a | a <- xs, a < x || a == x]
              larger = [b | b <- xs, b > x ]
    test xs = do print(Firststeps.last xs)
                 do print(Firststeps.lastix xs)
                    do print(Firststeps.init xs)
                       do print(Firststeps.prod xs)
                          do print(Firststeps.qsort xs)
                             do print(Firststeps.qsort [4,2,3,4,5,1,6,7])
    main = test [1,2..5]
