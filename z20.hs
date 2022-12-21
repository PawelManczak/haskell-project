-- liczba obfita to taka, w ktorej suma dzielnikow jest wieksza niz ona sama

-- generuje tablice liczb obfitych mniejszych niz n
-- liczy sume dzielnikow i sprawdza, czy jest wieksza niz sama liczba
liczbyObfite :: Int -> [Int]
liczbyObfite n = [i | i <- [1..n], sum (filter (\x -> i `mod` x == 0) [1..i-1]) > i]
-- [element do dodania do wynikowej tablicy | element sprawdzany <- lista elementów do sprawdzenia, warunek]

-- troche dziwne, ale dziala
-- chodzi o to, ze nasze "i" jest sumą i jeśli istnieje takie, że i - j=x
 -- , a x należy do tej tablcy to -> i = x +j, czyli się zgadza
skladajaceSieZSumyDwochElementowTab :: Int -> [Int] -> [Int]
skladajaceSieZSumyDwochElementowTab n tab = filter (\i -> any (\j -> (i - j) `elem` tab) tab) [1..n-1]
-- i: liczba od [1..n-1] - nasza suma
-- sprawdzamy, czy istnije taka liczba, że suma - liczbaObfita1 = liczbaObfita2
-- co oznacza, że suma może być zapisana sumą dwóch liczb obfitych


main :: IO ()
n = 100
tabLiczbObfitych = liczbyObfite n
main = print $ skladajaceSieZSumyDwochElementowTab n tabLiczbObfitych
