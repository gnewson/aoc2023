distances :: Int -> [Int]
distances time = [(time - hold) * hold | hold <- [0..time] ]


greaterDistances :: Int -> Int -> Int
greaterDistances time distance = length . filter (>distance) . distances $ time

-- for solution
-- foldl (*) 1 over the list of distances
