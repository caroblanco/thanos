module Lib where
import Text.Show.Functions

type Gema = Personaje -> Personaje

data Guantelete = UnGuantelete {
    material :: String,
    gemas :: [Gema]
}

data Personaje = UnPersonaje {
    edad:: Int,
    energia :: Int,
    habilidades::[String],
    nombre::String,
    planeta::String
}

type Universo = [Personaje]

chasquidoDeUniverso :: Universo -> Guantelete -> Universo
chasquidoDeUniverso personajes guantelete 
    |guanteleteCompleto guantelete = take (length personajes `div` 2) personajes
    | otherwise = personajes

---------1
guanteleteCompleto :: Guantelete -> Bool
guanteleteCompleto guantelete = ((==6).length.gemas) guantelete && ((=="uru").material) guantelete

---------2
aptoParaPendex :: Universo -> Bool
aptoParaPendex = any ((<45).edad)

energiaTotal :: Universo -> Int
energiaTotal = sum. map energia. filter ((>1).length.habilidades)

---------3
cambiarEnergia :: (Int -> Int) -> Personaje -> Personaje
cambiarEnergia f personaje = personaje {energia = f (energia personaje)}

cambiarEdad:: (Int -> Int) -> Personaje -> Personaje
cambiarEdad f personaje = personaje {edad = f (edad personaje)}

cambiarHabilidad :: ([String] -> [String]) -> Personaje -> Personaje
cambiarHabilidad f personaje = personaje {habilidades = f (habilidades personaje)}

cambiarPlaneta :: (String -> String) -> Personaje -> Personaje
cambiarPlaneta f personaje = personaje {planeta = f (planeta personaje)}

restar :: Int -> Int -> Int
restar resto valor = valor-resto

laMente :: Int-> Gema
laMente valor = cambiarEnergia (restar valor)

elAlma :: String -> Gema
elAlma habilidad = cambiarHabilidad (\habilidades -> filter ((/=) habilidad) habilidades) . cambiarEnergia (restar 10)

elEspacio :: String -> Gema
elEspacio planetaNuevo = cambiarPlaneta (\_ -> planetaNuevo). cambiarEnergia (restar 20)

elPoder :: Gema
elPoder = cambiarEnergia (\_->0).cambiarHabilidad condicionHabilidades

condicionHabilidades :: [String] -> [String]
condicionHabilidades habilidades
    |menosQue2 habilidades = []
    |otherwise = habilidades

menosQue2 :: [String] -> Bool
menosQue2 = (<=2).length

elTiempo :: Gema
elTiempo = cambiarEdad mitadEdad . cambiarEnergia (restar 50)

mitadEdad :: Int -> Int
mitadEdad edad = max 18 (div edad 2)

gemaLoca :: Gema -> Gema
gemaLoca gema = gema.gema

-------4
guanteleteEj = UnGuantelete {material = "goma", gemas = [elTiempo, elAlma "usar Mjolnir",gemaLoca (elAlma "programacion en Haskell")]}

--------5
utilizar :: Personaje -> [Gema] -> Personaje
utilizar personaje = foldr ($) personaje

-------6
gemaMasPoderosa :: Personaje -> Guantelete -> Gema
gemaMasPoderosa personaje = maximizarSegun personaje . gemas 

maximizarSegun :: Personaje -> [Gema] -> Gema
maximizarSegun _ [gema] = gema
maximizarSegun personaje (x:y:xs) 
    |g1Ganag2 personaje x y =  maximizarSegun personaje (x:xs)
    |otherwise = maximizarSegun personaje (y:xs)

g1Ganag2 :: Personaje -> Gema -> Gema -> Bool
g1Ganag2 personaje g1 g2 = (energia.g1) personaje < (energia.g2) personaje

{- maximizarSegun :: Personaje -> [Gema] -> Gema
maximizarSegun personaje = foldl1 (maxSegun personaje)

maxSegun :: Personaje -> Gema -> Gema -> Gema
maxSegun personaje g1 g2
    |g1Ganag2 personaje g1 g2 = g1
    |otherwise = g1

g1Ganag2 :: Personaje -> Gema -> Gema -> Bool
g1Ganag2 personaje g1 g2 = energiaConGema g1 personaje < energiaConGema g2 personaje

energiaConGema :: Gema -> Personaje -> Int
energiaConGema gema = energia.gema -}

----------7
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = UnGuantelete "vesconite" (infinitasGemas elTiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (flip utilizar . take 3. gemas) guantelete

{-
gemaMasPoderosa punisher guanteleteDeLocos


usoLasTresPrimerasGemas guanteleteDeLocos punisher
esta funcionara, debido al concepto de lazy evaluation, aunque las gemas sean infinitas, haskell
interpreta primero que solo va a tomar ls tres primeras, por lo que no se queda colgado.
-}
spiderMan :: Personaje
spiderMan = UnPersonaje{
    edad = 21,
    energia = 2300,
    habilidades = ["sentido arácnido", "sacar fotos", "trepar paredes"],
    nombre = "Peter Parker",
    planeta = "tierra"
}    