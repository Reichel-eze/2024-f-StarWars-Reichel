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



