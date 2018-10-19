data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 1000000)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane x = not $ isCar x

areCars :: [Vehicle] -> [Bool]
areCars xs = map isCar xs

getManu :: Vehicle -> Maybe Manufacturer
getManu (Car x _) = Just x
getManu _ = Nothing

main = do
    print myCar
    print urCar
    print clownCar
    print doge
    print $ isCar myCar
    print $ isCar urCar
    print $ isCar doge
    print $ isPlane doge
    print $ isPlane myCar
    print $ getManu myCar
    print $ getManu doge

