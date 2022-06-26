import Text.Show.Functions

data Auto = UnAuto {
    color :: String,
    velocidad :: Int,
    distanciaRecorrida :: Int
} deriving (Show , Eq, Ord)

type Carrera = [Auto]
mapColor :: (String -> String) -> Auto -> Auto
mapColor unaFuncion unAuto = unAuto {color = unaFuncion.color $ unAuto}

mapVelocidad :: (Int -> Int) -> Auto -> Auto
mapVelocidad unaFuncion unAuto = unAuto { velocidad = unaFuncion.velocidad $ unAuto}

mapDistanciaRecorrida :: (Int -> Int) -> Auto -> Auto
mapDistanciaRecorrida unaFuncion unAuto = unAuto {distanciaRecorrida = unaFuncion.distanciaRecorrida $ unAuto}

-- PUNTO 1

sonDistintos :: Auto -> Auto -> Bool
sonDistintos unAuto otroAuto = unAuto /= otroAuto

estaCerca :: Auto -> Auto -> Bool
estaCerca unAuto otroAuto = hayCercanos unAuto otroAuto && sonDistintos unAuto otroAuto

hayCercanos :: Auto -> Auto -> Bool
hayCercanos unAuto otroAuto = distanciaRecorrida.mapDistanciaRecorrida (distanciaRecorrida otroAuto -) $ unAuto

distanciaMenorA10 :: Auto -> Bool
distanciaMenorA10 unAuto = (<10).abs.distanciaRecorrida $ unAuto

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo unAuto unaCarrera = any (estaCerca unAuto) unaCarrera && estaPrimero unAuto unaCarrera

estaPrimero :: Auto -> Carrera -> Bool
estaPrimero unAuto unaCarrera = all (< distanciaRecorrida unAuto).map distanciaRecorrida $ unaCarrera

puesto :: Auto -> Carrera -> Int
puesto unAuto unaCarrera = 1 + length.filter (estaAdelante unAuto) $ unaCarrera

estaAdelante :: Auto -> Auto -> Bool
estaAdelante unAuto otroAuto = distanciaRecorrida unAuto < distanciaRecorrida otroAuto

-- PUNTO 2

corra :: Int -> Auto -> Auto
corra tiempo unAuto = mapDistanciaRecorrida (+ tiempo * velocidad unAuto) unAuto

alterarVelocidad :: (Int -> Int) -> Auto -> Auto
alterarVelocidad = mapVelocidad

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad unaVelocidad unAuto = alterarVelocidad (bajandoVelocidadA unaVelocidad) unAuto

bajandoVelocidadA :: Int -> Int -> Int
bajandoVelocidadA otraVelocidad unaVelocidad = max 0 (unaVelocidad - otraVelocidad)

-- PUNTO 3
type PowerUp = Auto -> Carrera -> Carrera
afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: PowerUp
terremoto unAuto unaCarrera = afectarALosQueCumplen (estaCerca unAuto) (bajarVelocidad 50) unaCarrera

miguelitos :: Int -> PowerUp
miguelitos  unaVelocidad unAuto unaCarrera = afectarALosQueCumplen (estaAdelante unAuto) (bajarVelocidad unaVelocidad) unaCarrera

jetPack :: Int -> PowerUp
jetPack unTiempo unAuto unaCarrera = afectarALosQueCumplen (== unAuto) (doblarVelocidadDurante unTiempo) unaCarrera

doblarVelocidadDurante :: Int -> Auto -> Auto
doblarVelocidadDurante unTiempo unAuto = alterarVelocidad (flip div 2).corra unTiempo. alterarVelocidad (*2) $ unAuto
 
-- PUNTO 4

type Evento = Carrera -> Carrera
type Color = String

simularCarrera :: Carrera -> [Evento] -> [(Int , Color)]
simularCarrera unaCarrera unosEventos =  entuplar.aplicarEventos unosEventos $ unaCarrera

aplicarEventos :: [Evento] -> Carrera -> Carrera
aplicarEventos unosEventos unaCarrera = foldl (\x y -> y x) unaCarrera unosEventos 

entuplar :: Carrera -> [(Int , Color)]
entuplar unaCarrera = map (puestoYColor unaCarrera) unaCarrera

puestoYColor ::  Carrera -> Auto -> (Int , Color)
puestoYColor unaCarrera unAuto  = (puesto unAuto unaCarrera, color unAuto)

correnTodos :: Int -> Evento
correnTodos unTiempo unaCarrera = map (corra unTiempo) unaCarrera

usaPowerUp :: PowerUp -> Color -> Carrera -> Carrera
usaPowerUp unPowerUp unColor unaCarrera = unPowerUp (encontrarAuto unColor unaCarrera) unaCarrera

encontrarAuto :: Color -> Carrera -> Auto
encontrarAuto unColor unaCarrera = head.filter (esDeColor unColor) $ unaCarrera

esDeColor :: Color -> Auto -> Bool
esDeColor unColor unAuto = color unAuto == unColor