

data Personaje = Personaje {
nombrePersonaje :: String,
cantidadDePoder :: Int,
listaDeDerrotas :: [Derrota],
equipamientos :: [Equipamiento]
}
type Equipamiento = Personaje -> Personaje
type Derrota = (String, Float)

oponente :: Derrota -> String
oponente = fst

anio :: Derrota -> Float
anio = snd

-- PARTE B -- 2

entrenamiento :: [Personaje] -> [Personaje]
entrenamiento = aumentarPoderPersonajes 

aumentarPoderPersonajes :: [Personaje] -> [Personaje]
aumentarPoderPersonajes grupoDePersonajes = map (aumentoDePoderIndividual (length grupoDePersonajes)) grupoDePersonajes

aumentoDePoderIndividual:: Int -> Personaje -> Personaje 
aumentoDePoderIndividual cantEntrenando = mapPoder (*cantEntrenando) 

mapPoder :: (Int -> Int) -> Personaje -> Personaje 
mapPoder f unPersonaje = unPersonaje {cantidadDePoder = f $ cantidadDePoder unPersonaje}


-- PARTE B -- 3

rivalesDignos :: [Personaje] -> [Personaje]
rivalesDignos grupoDePersonajes = filter esDigno (entrenamiento grupoDePersonajes)

esDigno :: Personaje -> Bool 
esDigno unPersonaje = estaFuerte unPersonaje && tieneDerrota "Hijo de thanos" unPersonaje


estaFuerte :: Personaje -> Bool 
estaFuerte unPersonaje =  500 < cantidadDePoder unPersonaje

tieneDerrota :: String -> Personaje -> Bool 
tieneDerrota unaDerrota unPersonaje =  elem unaDerrota  (derrotasDelPersonaje unPersonaje)

derrotasDelPersonaje :: Personaje -> [String]
derrotasDelPersonaje unPersonaje = map oponente (listaDeDerrotas unPersonaje)


-- PUNTO B -- 4
type Anio = Float 
guerraCivil :: Anio -> [Personaje] -> [Personaje] -> [Personaje]
guerraCivil unAnio = zipWith (peleaIndividual unAnio)

peleaIndividual :: Anio -> Personaje -> Personaje -> Personaje 
peleaIndividual unAnio personaje1 personaje2 
    | cantidadDePoder personaje1 > cantidadDePoder personaje2 = agregarDerrota (nombrePersonaje personaje2, unAnio) personaje1 
    | otherwise = agregarDerrota (nombrePersonaje personaje1, unAnio) personaje2 

agregarDerrota :: Derrota -> Personaje -> Personaje
agregarDerrota unaDerrota = mapDerrotas (unaDerrota :) 

mapDerrotas :: ([Derrota] -> [Derrota]) ->Personaje -> Personaje 
mapDerrotas f personajeGanador = personajeGanador {listaDeDerrotas = f $ listaDeDerrotas personajeGanador}

-- PARTE B -- 2

escudo :: Personaje -> Personaje 
escudo unPersonaje 
    | cantidadDerrotas unPersonaje > 5 = aumentarPoder 50 unPersonaje 
    | otherwise = disminuirPoder 100 unPersonaje 

    where cantidadDerrotas = length.listaDeDerrotas 


aumentarPoder :: Int  -> Personaje -> Personaje 
aumentarPoder cantidad = mapPoder (+ cantidad) 

disminuirPoder :: Int -> Personaje -> Personaje 
disminuirPoder cantidad  = mapPoder (subtract cantidad)

trajeMecanizado :: String-> Personaje -> Personaje 
trajeMecanizado version = ponerleVersion version.hacerIron 


ponerleVersion :: String -> Personaje -> Personaje 
ponerleVersion version = mapNombrePersonaje (++ version) 

hacerIron :: Personaje -> Personaje 
hacerIron = mapNombrePersonaje ("iron " ++) 

mapNombrePersonaje ::  (String -> String) -> Personaje -> Personaje 
mapNombrePersonaje f unPersonaje = unPersonaje {nombrePersonaje = f $ nombrePersonaje unPersonaje} 

-- PARTE 3 -- a 

stormBreaker :: Equipamiento 
stormBreaker unPersonaje 
    | nombrePersonaje unPersonaje == "Thor" = (mapDerrotas (drop (length (listaDeDerrotas unPersonaje))).mapNombrePersonaje (++ " dios del trueno")) unPersonaje
    | otherwise = unPersonaje 

--gemaDelAlma :: Equipamiento 
--gemaDelAlma unPersonaje 
  --  | nombrePersonaje unPersonaje == "Thanos" = agregarDerrota 


--derrotaExtras :: [Derrota]
--derrotaExtras = map

--guanteleteInfinito ::  [Equipamiento] ->  Equipamiento 
--guanteleteInfinito unEquipamiento unPersonaje = foldl (\x f -> f x) unPersonaje (equipamientosGema equipamientos)

--equipamientosGema :: [Equipamiento] -> [Equipamiento] 
--equipamientosGema equipamientos =  filter esGemaDelInfinito equipamientos 

-- PARTE C -- 
