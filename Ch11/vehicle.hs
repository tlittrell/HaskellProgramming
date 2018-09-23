-- vehicle.hs

module Vehicle where

data Price = Price Integer deriving (Eq, Show)

data Size = Size Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsRUs | TakeYourChancesUnited 
    deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size
    deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 100)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _         = False

areCars :: (Vehicle -> Bool) -> [Vehicle] -> [Bool]
areCars f = map f

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu
getManu _            = error "cannot get manufacturer of a plane"

getPrice :: Vehicle -> Integer
getPrice (Car _ (Price i)) = i
getPrice _                 = error "cannot get price of a plane"

main :: IO ()
main = do
    print $ isCar myCar == True
    print $ isCar doge == False
    print $ isPlane myCar == False
    print $ isPlane doge == True
    print $ areCars isCar [myCar, urCar, doge] == [True, True, False]
    print $ getManu myCar == Mini