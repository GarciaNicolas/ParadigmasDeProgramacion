data Jugador = UnJugador {
    nombre :: String,
    padre :: String,
    habilidad :: Habilidad
} deriving (Show, Eq)

data Habilidad = Habilidad {
    fuerza :: Int,
    precision :: Int
} deriving (Show, Eq)

data Tiro = UnTiro{
    velocidad :: Int,
    precisionTiro :: Int,
    altura :: Int
} deriving (Show, Eq)

type Puntos = Int

----1
type Palo = Habilidad -> Tiro

putter :: Palo
putter unaHabilidad = UnTiro {
    velocidad = 10,
    precisionTiro = precision unaHabilidad * 2,
    altura = 0}


madera :: Palo
madera unaHabilidad = UnTiro {
    velocidad = 100,
    precisionTiro = div (precision unaHabilidad) 2,
    altura = 5}

hierros :: Int -> Palo
hierros n unaHabilidad = UnTiro {
    velocidad = fuerza unaHabilidad * n,
    precisionTiro = div (precision unaHabilidad) n,
    altura = max (n-3) 0} 

palos :: [Palo]
palos = [putter, madera] ++ map hierros [1..10]


---2
golpe :: Jugador -> Palo -> Tiro
golpe unJugador unPalo = unPalo (habilidad unJugador)

---3

type Obstaculo = Tiro -> Bool

obstaculoA :: Obstaculo
obstaculoA unTiro = precisionTiro unTiro > 90 && altura unTiro == 0

obstaculoB :: Obstaculo
obstaculoB unTiro = velocidad unTiro > 80 && (altura unTiro <5 && altura unTiro >1)

obstaculoC :: Obstaculo
obstaculoC unTiro = (velocidad unTiro > 5 && velocidad unTiro < 20) && altura unTiro == 0 && precisionTiro unTiro > 95

pasaObstaculo :: Int -> Tiro -> Obstaculo -> Tiro
pasaObstaculo alturaLaguna unTiro unObstaculo
    |unObstaculo unTiro = luegoDelObstaculo alturaLaguna unTiro unObstaculo
    |otherwise = UnTiro {
        velocidad = 0,
        precisionTiro = 0,
        altura = 0
    }

luegoDelObstaculo :: Int -> Tiro -> Obstaculo -> Tiro
luegoDelObstaculo _ unTiro obstaculoA = UnTiro {
    velocidad = velocidad unTiro *2,
    precisionTiro = 100,
    altura = 0
}
luegoDelObstaculo alturaLaguna unTiro obstaculoB = UnTiro {
    velocidad = velocidad unTiro,
    precisionTiro = precisionTiro unTiro,
    altura = div (altura unTiro) alturaLaguna
}
luegoDelObstaculo _ unTiro obstaculoC = UnTiro {
    velocidad = 0,
    precisionTiro = 0,
    altura = 0
}
luegoDelObstaculo _ unTiro obstaculoX = unTiro


--4

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador unObstaculo = filter (filtrarObstaculos unObstaculo unJugador) palos

filtrarObstaculos :: Obstaculo -> Jugador -> Palo -> Bool
filtrarObstaculos unObstaculo unJugador unPalo = unObstaculo.(golpe unJugador) $ unPalo

superarObstaculos :: [Obstaculo] -> Tiro -> Int
superarObstaculos [] _ = 0
superarObstaculos (x:xs) unTiro
    |x unTiro = 1 + superarObstaculos xs (luegoDelObstaculo 0 unTiro x)
    |otherwise = 0

tiroNoNulo :: Tiro -> Bool
tiroNoNulo unTiro = velocidad unTiro /= 0 && precisionTiro unTiro /= 0 && altura unTiro /= 0

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil unJugador listaObstaculos = maximoSegun (superarObstaculos listaObstaculos.golpe unJugador) palos


maximoSegun :: Ord b => (a->b) -> [a] -> a
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord b => (a->b) -> a -> a -> a
mayorSegun f a b
  | f a > f b = a
  | otherwise = b
