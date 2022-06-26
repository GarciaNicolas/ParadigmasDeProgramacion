data Cancion = UnaCancion {
    titulo :: Titulo,
    genero :: Genero,
    duracion :: Int
}

type Titulo = String
type Genero = String

data Artista = UnArtista {
    nombre :: Nombre,
    canciones :: [Cancion],
    efecto :: Efecto
}

type Nombre = String
type Efecto = Cancion -> Cancion


mapTitulo :: (Titulo -> Titulo ) -> Cancion -> Cancion
mapTitulo unaFuncion unaCancion = unaCancion {titulo = unaFuncion.titulo $ unaCancion}

mapGenero :: (Genero -> Genero ) -> Cancion -> Cancion
mapGenero unaFuncion unaCancion = unaCancion {genero = unaFuncion.genero $ unaCancion}

mapDuracion :: (Int -> Int ) -> Cancion -> Cancion
mapDuracion unaFuncion unaCancion = unaCancion {duracion = unaFuncion.duracion $ unaCancion}

-- PARTE A-1

acortar :: Efecto
acortar unaCancion = mapDuracion (subtract 60) unaCancion

remixar :: Efecto
remixar unaCancion = mapTitulo (++ " remix").mapDuracion (*2).mapGenero (const "remixado") $ unaCancion

acustizar :: Int -> Efecto
acustizar unaDuracion (UnaCancion titulo "acústico" duracion) =  (UnaCancion titulo "acústico" duracion)
acustizar unaDuracion unaCancion = mapGenero (const "acústico").mapDuracion (const unaDuracion) $ unaCancion

metaEfecto :: [Efecto] -> Cancion -> Cancion
metaEfecto listaEfectos unaCancion = foldr (\x y -> x y) unaCancion listaEfectos

-- PARTE A

cafeParaDos :: Cancion
cafeParaDos = UnaCancion{
    titulo = "Café para dos",
    genero = "rock melancólico",
    duracion = 146
}

fuiHastaAhi :: Cancion
fuiHastaAhi = UnaCancion{
    titulo = "Fuí hasta ahí",
    genero = "rock",
    duracion = 279
}

rocketRaccoon :: Cancion
rocketRaccoon = undefined

mientrasMiBateriaFesteja :: Cancion
mientrasMiBateriaFesteja = undefined

tomateDeMadera :: Cancion
tomateDeMadera = undefined

losEscarabajos :: Artista
losEscarabajos = UnArtista {
    nombre = "Los Escarabajos",
    canciones = [rocketRaccoon, mientrasMiBateriaFesteja, tomateDeMadera],
    efecto = acortar
}

unPibeComoVos :: Cancion
unPibeComoVos = undefined

daleMechaALaLluvia :: Cancion
daleMechaALaLluvia = undefined

adela :: Artista
adela = UnArtista {
    nombre = "Adela",
    canciones = [unPibeComoVos, daleMechaALaLluvia],
    efecto = remixar
}

elTigreJoaco :: Artista
elTigreJoaco = UnArtista {
    nombre = "El tigre Joaco",
    canciones = [],
    efecto = acustizar 6
}

-- PARTE B

type Playlist = [Cancion]

vistazo :: Artista -> Playlist
vistazo unArtista = take 3.filter esCorta.canciones $ unArtista

esCorta :: Cancion -> Bool
esCorta unaCancion = duracion unaCancion < 150

playlist :: Genero -> [Artista] -> Playlist
playlist unGenero unosArtistas = foldr ((++).filtrarPorGenero unGenero) [] unosArtistas

filtrarPorGenero :: Genero -> Artista -> Playlist
filtrarPorGenero unGenero unArtista = filter (esDeGenero unGenero).canciones $ unArtista

esDeGenero :: Genero -> Cancion -> Bool
esDeGenero unGenero unaCancion = genero unaCancion == unGenero

-- PARTE C

hacerseDJ :: Artista -> Artista
hacerseDJ unArtista = unArtista { canciones = map (efecto unArtista) (canciones unArtista)}

tieneGustoHomogeneo :: Artista -> Bool
tieneGustoHomogeneo (UnArtista _ canciones _) = all (esDeGenero.genero.head $ canciones) canciones


formarBanda :: Nombre -> [Artista] -> Artista
formarBanda unNombre unosArtistas = UnArtista {
    nombre = unNombre,
    canciones = unirCanciones unosArtistas,
    efecto = combinarEfectos unosArtistas
}

unirCanciones :: [Artista] -> [Cancion]
unirCanciones unosArtistas = foldr1 (++).map canciones $ unosArtistas

combinarEfectos :: [Artista] -> Efecto
combinarEfectos unosArtistas = foldr1 (.).map efecto $ unosArtistas

obraMaestraProgesiva :: Artista -> Cancion
obraMaestraProgesiva unArtista = UnaCancion {
    titulo = foldl1 (++).map titulo.canciones $ unArtista,
    genero = foldl1 generoSuperador.map genero.canciones $ unArtista,
    duracion = sum.map duracion.canciones $ unArtista
} 

generoSuperador :: Genero -> Genero -> Genero
generoSuperador "rock" _ = "rock"
generoSuperador _ "rock" = "rock"
generoSuperador "reggaeton" otroGenero = otroGenero
generoSuperador otroGenero "reggaeton" = otroGenero
generoSuperador unGenero otroGenero = esMasLargo unGenero otroGenero

esMasLargo :: Genero -> Genero -> Genero
esMasLargo unGenero otroGenero 
    | length unGenero > length otroGenero = unGenero
    | otherwise = otroGenero

{- PARTE D
1- Sí
2- Sí
3- Sí-}
