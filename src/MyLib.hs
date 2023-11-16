module MyLib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = xs -- [x]

-- Напишите тесты к функции и функцию
--
-- Работает как zip, но если один список
-- длиннее, циклически переиспользует второй
--
-- > zipLong [1,2,3] "abc"
-- [(1,'a'),(2,'b'),(3,'c')]
--
-- > zipLong [1,2] "abcd"
-- [(1,'a'),(2,'b'),(1,'c'),(2,'d')]
--
-- > zipLong [] "abcd"
-- []

zipLong :: [a] -> [b] -> [(a, b)]
zipLong [] _ = []
zipLong _ [] = []
zipLong as bs = zipWith (\a b -> (a, b)) as (cycle bs)

-- zipLong1 :: [a] -> [b] -> [(a, b)]
-- zipLong1 [] _ = []
-- zipLong1 _ [] = []
-- zipLong1 as bs = zipWith (\a b -> (a, b)) (cycle as) bs

zipCycle :: [a] -> [b] -> [(a, b)]
zipCycle xs ys = zipCycle' (cycle xs) ys
  where
    zipCycle' [] _ = []
    zipCycle' _ [] = []
    zipCycle' (x:xs') (y:ys') = (x, y) : zipCycle' xs' ys'
