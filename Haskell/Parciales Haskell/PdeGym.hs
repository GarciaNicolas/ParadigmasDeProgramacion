
data Persona = UnaPersona {
    nombre :: Nombre,
    calorias :: Calorias,
    hidratacion :: Hidratacion,
    tiempoDisponible :: Tiempo,
    equipamientos :: [Equipamiento]

} deriving (Show , Eq, Ord)



type Equipamiento = String
type Nombre = String
type Hidratacion = Int
type Calorias = Int
type Tiempo = Int

--Parte A1

type Ejercitar = Repeticiones -> Persona -> Persona
type Repeticiones = Int

transpirar :: Hidratacion -> Hidratacion -> Repeticiones -> Repeticiones -> Hidratacion
transpirar hidratacionPersona hidratacionAPerder repeticionesAPerder repeticionesHechas = hidratacionPersona - (div repeticionesAPerder repeticionesHechas * hidratacionAPerder)

abdominales :: Ejercitar
abdominales repeticiones unaPersona = 
    unaPersona {calorias = calorias unaPersona - 8 * repeticiones}

flexiones ::  Ejercitar
flexiones repeticiones unaPersona = 
    unaPersona {calorias = calorias unaPersona - 16 * repeticiones, hidratacion = transpirar (hidratacion unaPersona) 2 10 repeticiones}

tienePesa :: Persona -> Bool
tienePesa unaPersona = elem "pesa" (equipamientos unaPersona)

levantarPesas :: Int -> Ejercitar
levantarPesas unPeso repeticiones unaPersona 
    |tienePesa unaPersona = unaPersona {calorias = calorias unaPersona - 32 * repeticiones, hidratacion = transpirar (hidratacion unaPersona) unPeso 10 repeticiones}
    |otherwise = unaPersona

laGranHomero :: Persona -> Persona
laGranHomero = id

-- Parte A2

type Accion = Persona -> Persona

renovarEquipo :: Equipamiento -> Accion
renovarEquipo unEquipamiento unaPersona = unaPersona {equipamientos = unEquipamiento : (equipamientos unaPersona)}

volverseYoguista :: Accion 
volverseYoguista unaPersona = unaPersona {calorias = div (calorias unaPersona) 2, hidratacion = min (hidratacion unaPersona) 100, equipamientos = ["colchoneta"]}

volverseBodyBuilder :: Accion
volverseBodyBuilder unaPersona
    |all (=="pesa") (equipamientos unaPersona) = unaPersona {nombre = nombre unaPersona ++ " BB", calorias = calorias unaPersona * 3}
    |otherwise = unaPersona

comerUnSandwich :: Accion
comerUnSandwich unaPersona = unaPersona {calorias = calorias unaPersona + 500, hidratacion = 100}

-- Parte B
data Rutina = UnaRutina {
    tiempoEstimado :: Tiempo,
    ejercicios :: [Ejercitar]
} 

hacerRutina :: Rutina -> Persona -> Persona
hacerRutina unaRutina unaPersona = foldl (\x y -> flip y x) unaPersona (ejercicios unaRutina)

nivelDeCaloriasEHidratacion :: Persona -> (Calorias -> Bool) -> (Hidratacion -> Bool) -> Bool
nivelDeCaloriasEHidratacion unaPersona nivelCaloria nivelHidratacion = (calorias unaPersona) nivelCaloria && (hidratacion unaPersona) nivelHidratacion

esPeligrosa :: Rutina -> Persona -> Bool
esPeligrosa unaRutina unaPersona = nivelDeCaloriasEHidratacion (hacerRutina unaRutina unaPersona) (<50) (<10)

esBalanceada :: Persona -> Bool
esBalanceada unaRutina unaPersona = nivelDeCaloriasEHidratacion (hacerRutina unaRutina unaPersona) (div (calorias unaPersona) 2) (>80)

elAbominableAbdominal :: Rutina
elAbominableAbdominal = (1, abdominales [0..])

-- Parte C

seleccionarGrupoDeEjercicio :: Persona -> [Persona] -> [Persona]
seleccionarGrupoDeEjercicio  unaPersona grupoPersonas = filter (mismoTiempoDisponible unaPersona) grupoPersonas

mismoTiempoDisponible :: Persona -> Persona -> Bool
mismoTiempoDisponible unaPersona otraPersona = tiempoDisponible unaPersona == tiempoDisponible otraPersona

promedioDeRutina :: Rutina -> [Persona] -> (Calorias , Hidratacion)
promedioDeRutina unaRutina grupoPersonas = informar.map (hacerRutina unaRutina) $ grupoPersonas

informar :: [Persona] -> (Calorias, Hidratacion)
informar grupoPersonas = ( sumarCalorias grupoPersonas `div` length grupoPersonas , (sumarHidratacion grupoPersonas) `div` (length grupoPersonas))

sumarCalorias :: [Persona] -> Calorias
sumarCalorias grupoPersonas = foldl (\x unaPersona -> (calorias unaPersona) + x) 0 grupoPersonas

sumarHidratacion :: [Persona] -> Hidratacion
sumarHidratacion grupoPersonas = foldl (\x unaPersona -> (hidratacion unaPersona) + x) 0 grupoPersonas