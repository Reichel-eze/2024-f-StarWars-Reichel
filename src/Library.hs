module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- 1) Modelar las naves espaciales mencionadas y agregar una nueva nave, 
-- con un poder especial sutilmente diferente a alguna de las anteriores, en el que se aproveche las otras implementaciones.

data Nave = UnaNave {
    nombre :: String,
    durabilidad :: Number,
    escudo :: Number,
    ataque :: Number,
    poderes :: [Poder]
}deriving (Show,Eq)

-- MODIFICACIONES CAMPOS --

--restaNoNegativa :: Number -> Number -> Number
--restaNoNegativa n m = max 0 (n - m)

modificarAtaque :: Number -> Nave -> Nave
modificarAtaque valor nave = nave {ataque = ataque nave + valor}

disminuirAtaque :: Number -> Nave -> Nave
disminuirAtaque valor nave = nave {ataque = restaNoNegativa (ataque nave) valor}

modificarDurabiladad :: Number -> Nave -> Nave
modificarDurabiladad valor nave = nave {durabilidad = durabilidad nave + valor}

disminuirDurabiliad :: Number -> Nave -> Nave
disminuirDurabiliad valor nave = nave {durabilidad = restaNoNegativa (durabilidad nave) valor}

modificarEscudos :: Number -> Nave -> Nave
modificarEscudos valor nave = nave {escudo = escudo nave + valor}

-- CREACION DE NAVES --

tieFighter :: Nave
tieFighter = UnaNave "TIE Fighter" 200 100 50 [movimientoTurbo]

xWing :: Nave
xWing = UnaNave "X Wing" 300 150 100 [reparacionEmergencia]

naveDeDarthVader :: Nave
naveDeDarthVader = UnaNave "Nave de Darth Vader" 500 300 200 [movimientoSuperTurbo]

millenniumFalcon :: Nave
millenniumFalcon = UnaNave "Millennium Falcon" 1000 500 50 [reparacionEmergencia , modificarEscudos 100]

navePicantovich :: Nave 
navePicantovich = UnaNave "Nave Picantovich" 1000 1000 1000 [poderEspecial]

-- PODERES --

type Poder = Nave -> Nave

movimientoTurbo :: Poder
movimientoTurbo = modificarAtaque 25 

reparacionEmergencia :: Poder
reparacionEmergencia = modificarAtaque (-30) . modificarDurabiladad 50

movimientoSuperTurbo :: Poder
movimientoSuperTurbo = modificarDurabiladad (-45) . movimientoTurbo . movimientoTurbo . movimientoTurbo
 
poderEspecial :: Poder
poderEspecial = reparacionEmergencia . movimientoSuperTurbo

-- 2) Calcular la durabilidad total de una flota, formada por un conjunto de naves, 
-- que es la suma de la durabilidad de todas las naves que la integran.

type Flota = [Nave]

flotaEjemplo :: Flota
flotaEjemplo = [tieFighter,xWing]

durabilidadTotal :: Flota -> Number           -- 1ero) hago una lista de durabilidades en base a la lista de naves
durabilidadTotal = sum . map durabilidad      -- 2dos) hago la sumatoria entre las durabilidades de dicha lista de durabilidades

-- 3) Saber cómo queda una nave luego de ser atacada por otra. 
-- Cuando ocurre un ataque ambas naves primero activan su poder especial y luego la nave atacada reduce 
-- su durabilidad según el daño recibido, que es la diferencia entre el ataque de la atacante y el escudo de la atacada. 
-- (si el escudo es superior al ataque, la nave atacada no es afectada). 
-- La durabilidad, el escudo y el ataque nunca pueden ser negativos, a lo sumo 0.

--atacar :: Nave -> Nave -> Nave
--atacar naveAtacante naveAtacada = aplicarDañoRecibido (aplicarPoderesDeLaNave naveAtacante) (aplicarPoderesDeLaNave naveAtacada) 

--aplicarDañoRecibido :: Nave -> Nave -> Nave
--aplicarDañoRecibido naveAtacante naveAtacada = dañarDurabiladad (dañoRecibido naveAtacada naveAtacante) naveAtacada

--danioRecibido' = ataque atanque - escudo atacada
--danioRecibido' :: Nave -> Nave -> Number
--danioRecibido' naveAtacante naveAtacada 
--    | ataque naveAtacante > escudo naveAtacada = ataque naveAtacante - escudo naveAtacada       
--    | otherwise = 0           

atacar :: Nave -> Nave -> Nave                                          -- 6to) aplico el daño pero esta vez con las naves luego de haberse aplicado sus poderes    
atacar naveAtacante naveAtacada = aplicarDanio (aplicarPoderesDeLaNave naveAtacante) (aplicarPoderesDeLaNave naveAtacada)

aplicarDanio :: Nave -> Nave -> Nave                                    -- 5to) aplico el daño con el valor del dañoRecibido en la Nave atacada                             
aplicarDanio naveAtacante naveAtacada = dañarDurabilidad (dañoRecibido naveAtacante naveAtacada) naveAtacada

dañarDurabilidad :: Number -> Nave -> Nave  -- para modifcar el campo durabilidad!! -- 4ero) Funcion auxilar, para poder modificar el campo durabilidad de la naveAtacada 
dañarDurabilidad n nave = nave {durabilidad = restaNoNegativa (durabilidad nave) n} -- porque la durabilidad NO puede dar negativo

-- danioRecibido = ataque atanque - escudo atacada                                                      -- 3eros) busco el valor del danio que va a recibir la naveAtacada
dañoRecibido :: Nave -> Nave -> Number                                                               
dañoRecibido naveAtacante naveAtacada = restaNoNegativa (ataque naveAtacante) (escudo naveAtacada)      -- si da negativa la resta --> ataque < escudo --> (me quedo con 0)
                                                                                                        -- si da positivo la resta --> ataque > escudo 
 
-- (si el escudo es superior al ataque, la nave atacada no es afectada)

restaNoNegativa :: Number -> Number -> Number                          -- FUNCION AUXILIAR : para evitar luego de una resta el resultado me de negativo (en ese caso, me entregara 0)
restaNoNegativa n m = max 0 (n - m)

aplicarPoderesDeLaNave :: Nave -> Nave                                 -- 2dos) aplico la lista de poderes sobre la nave                                      
aplicarPoderesDeLaNave nave = foldr (aplicarPoder) nave (poderes nave)  

aplicarPoder :: Poder -> Nave -> Nave                                  -- 1ero) aplico un solo poder sobre la nave
aplicarPoder poder nave = poder nave  

-- 4) Averiguar si una nave está fuera de combate, lo que se obtiene cuando su durabilidad llegó a 0.

fueraDeCombate :: Nave -> Bool
fueraDeCombate nave = durabilidad nave == 0

-- 5) Averiguar cómo queda una flota enemiga luego de realizar una misión sorpresa con una nave siguiendo una estrategia. 
-- Una estrategia es una condición por la cual la nave atacante decide atacar o no una cierta nave de la flota. 
-- Por lo tanto la misión sorpresa de una nave hacia una flota significa atacar todas aquellas naves de la flota que 
-- la estrategia determine que conviene atacar. Algunas estrategias que existen, y que deben estar reflejadas en la solución, son:
{-
1. Naves débiles: Son aquellas naves que tienen menos de 200 de escudo.
2. Naves con cierta peligrosidad: Son aquellas naves que tienen un ataque mayor a un valor dado. Por ejemplo, en alguna misión se podría utilizar una estrategia de peligrosidad mayor a 300, y en otra una estrategia de peligrosidad mayor a 100.
3. Naves que quedarían fuera de combate: Son aquellas naves de la flota que luego del ataque de la nave atacante quedan fuera de combate. 
4. Inventar una nueva estrategia -}

--mision sorpresa (nave + estrategia ) --> como que queda una flota enemiga (lista de naves)
--estrategia = condicion de la naveAtacante decide atacar o NO a una cierta nave de la flota 
--mision sorpresa (nave + estrategia) --> atacar TODAS aquellas naves de la flota que la estrategia determina que conviene atacar 

misionSorpresa :: Nave -> Estrategia -> Flota -> Flota
misionSorpresa naveAtacante estraregia flota = mapSelectivo (atacar naveAtacante) estraregia flota 

mapSelectivo :: (a -> a) -> (a -> Bool) -> [a] -> [a]  -- hago un cambio solo a los elementos de la lista que cumplen con una condicion y lo concatero con la lista de elementos que NO cumplen con la condicion
mapSelectivo cambio condicion lista = map cambio (filter condicion lista) ++ filter (not.condicion) lista

-- una estraregia es una condicion Por la cual la nave atacante decide atacar o no una cierta nave de la flota. (ES UN BOOLEANO)
type Estrategia = Nave -> Bool  -- (las estrategia son condiciones de las naves)

naveDebil :: Estrategia
naveDebil nave = escudo nave < 200

naveConCiertaPeliegrosidad :: Number -> Estrategia
naveConCiertaPeliegrosidad valor nave = ataque nave > valor

naveFueraDeCombate :: Nave -> Estrategia  -- la naveAtacada queda con la durabilidad en 0 luego del ataque
naveFueraDeCombate naveAtacante naveAtacada = fueraDeCombate (atacar naveAtacante naveAtacada)

-- 6) Considerando una nave y una flota enemiga en particular, dadas dos estrategias, determinar 
-- cuál de ellas es la que minimiza la durabilidad total de la flota atacada y llevar adelante una misión con ella

misionConMejorEstrategia :: Nave -> Estrategia -> Estrategia -> Flota -> Flota
misionConMejorEstrategia naveAtacante estrategia1 estrategia2 flota
    | tieneMenorDurabilidad naveAtacante estrategia1 estrategia2 flota = misionSorpresa naveAtacante estrategia1 flota
    | otherwise = misionSorpresa naveAtacante estrategia2 flota

tieneMenorDurabilidad :: Nave -> Estrategia -> Estrategia -> Flota -> Bool
tieneMenorDurabilidad naveAtacante estrategia1 estrategia2 flota = durabilidadTotal (misionSorpresa naveAtacante estrategia1 flota) < durabilidadTotal (misionSorpresa naveAtacante estrategia2 flota)

-- o tambien se puede hacer asi (pimero los guardas y que me devuelva la estrategia y despues la pongo en la misionSorpresa)

atarcarConMejorEstrategia :: Nave -> Estrategia -> Estrategia -> Flota -> Flota
atarcarConMejorEstrategia naveAtacante estrategia1 estrategia2 flota = misionSorpresa naveAtacante (mejorEstrategia naveAtacante estrategia1 estrategia2 flota) flota

mejorEstrategia :: Nave -> Estrategia -> Estrategia -> Flota -> Estrategia
mejorEstrategia naveAtacante estrategia1 estrategia2 flota
    | durabilidadTotal (misionSorpresa naveAtacante estrategia1 flota) < durabilidadTotal (misionSorpresa naveAtacante estrategia2 flota) = estrategia1
    | otherwise = estrategia2

-- 7) Construir una flota infinita de naves. 

flotaInfinita :: Flota
flotaInfinita = cycle [tieFighter,xWing]

-- ¿Es posible determinar su durabilidad total? 

--durabilidadTotal :: Flota -> Number           NO es posible determinar la durabilidad total de una lista infinita porque 
--durabilidadTotal = sum . map durabilidad      para determinar dicho campo necesito hacer la sumatoria de una lista de 
--                                              durabalidades que resulto infinita

-- ¿Qué se obtiene como respuesta cuando se lleva adelante una misión sobre ella? Justificar conceptualmente.
--misionSorpresa :: Nave -> Estrategia -> Flota -> Flota
--misionSorpresa naveAtacante estraregia flota = mapSelectivo (atacar naveAtacante) estraregia flota

-- Como la lista de naves es infinita va a poseer infinitas naves que cumplen con una condicon (y por eso seran atacadas) y 
-- otras infinitas naves que no cumplen con la condicion (y por eso no seran atacadas). Por lo tanto me devolvera como 
-- respuesta una flota con una cantidad de naves que fueron atacadas (pueden ser infinitas o 0) y una cantidad de naves 
-- que fueron NO fueron atacadas (pueden ser infinitas o 0). Tener en cuenta que AMBAS no pueden ser 0 porque una nave puede o NO 
-- puede cumplir la condicion (entonces es atacada o no) porque no existe una nave que cumpla y a la vez no cumpla la condicion.
-- Los casos que se pueden dar son :
--  * infinitas naves que cumplen con la condicion y cero que no cumplen (TODAS CUMPLEN)
--  * infinitas naves que NO cumplen con la condicion y cero que cumples (TODAS NO CUMPLEN)
--  * infinitas naves que cumplen e infinitas naves que NO cumplen (CUMPLEN O NO CUMPLEN)