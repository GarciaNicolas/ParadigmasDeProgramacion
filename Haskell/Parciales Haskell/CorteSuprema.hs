
data Ley = UnaLey {
    tema        :: Tema,
    presupuesto :: Int,
    grupos  :: [Grupo]
}

type Tema = String
type Grupo = String

sonCompatibles :: Ley -> Ley -> Bool
sonCompatibles unaLey otraLey = (any (comparteGrupo unaLey).grupos $ otraLey) && comparteTema (tema unaLey) (tema otraLey)

comparteGrupo :: Ley -> Grupo -> Bool
comparteGrupo unaLey unGrupo = elem unGrupo (grupos unaLey) 

comparteTema :: Tema -> Tema -> Bool
comparteTema [] _ = True
comparteTema _ [] = False
comparteTema unTema otroTema
    | unTema == take (length unTema) otroTema = True
    | otherwise = (comparteTema unTema).tail $ otroTema

-- PUNTO 2

type Juez = Ley -> Bool
type Agenda = [Tema]

esOpinionPublica :: Agenda -> Juez
esOpinionPublica unaAgenda unaLey = elem (tema unaLey) unaAgenda

esApoyadaPor :: Grupo -> Juez
esApoyadaPor unGrupo unaLey = (elem unGrupo).grupos $ unaLey

presupuestoMayorA :: Int -> Juez
presupuestoMayorA unPresupuesto unaLey = (>unPresupuesto).presupuesto $ unaLey

agenda :: Agenda
agenda = undefined

type CorteSuprema = [Juez]
corteSuprema :: CorteSuprema
corteSuprema = [
    esOpinionPublica agenda,
    esApoyadaPor "sector financiero",
    presupuestoMayorA 10,
    presupuestoMayorA 20,
    esApoyadaPor "partido conservador"]

nuevosJueces :: [Juez]
nuevosJueces = [ 
    const True,
    esApoyadaPor "partido nacional socialista",
    presupuestoMayorA 1]

agregarJueces :: CorteSuprema -> [Juez] -> CorteSuprema
agregarJueces unaCorte unosJueces = unaCorte ++ unosJueces

esConstitucional :: CorteSuprema -> Ley -> String
esConstitucional unaCorte unaLey 
    | esMayoria.juzgarLey unaLey $ unaCorte = "constitucional"
    | otherwise = "insconstitucional"

esMayoria :: [Bool] -> Bool
esMayoria votaciones = filter (==True) votaciones > filter (==False) votaciones

juzgarLey :: Ley -> CorteSuprema -> [Bool]
juzgarLey unaLey unaCorte = map ((\x y -> y x) unaLey) unaCorte

sonConstitucionales :: [Ley] -> CorteSuprema -> [Juez] -> [String]
sonConstitucionales unasLeyes unaCorte unosJueces =  map (esConstitucional (agregarJueces unaCorte unosJueces)) unasLeyes

borocotizar :: CorteSuprema -> CorteSuprema
borocotizar unaCorte = map (not.) unaCorte

juezCurioso :: String -> [Ley] -> Bool
juezCurioso unSector unasLeyes = all (esApoyadaPor unSector) unasLeyes