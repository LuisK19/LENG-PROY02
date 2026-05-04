module Main (main) where

import System.IO     (hSetBuffering, stdout, BufferMode(..))
import System.Random (randomRIO)
import Data.Time (getCurrentTime, utctDay, toGregorian,
                  fromGregorian, Day, dayOfWeek, DayOfWeek(..))

-- Estructuras de datos para eventos:
data Categoria
  = Visualizacion
  | Apartado
  | Compra
  | Devolucion
  | Seguimiento
  deriving (Show, Eq, Ord)

-- es como un struct
data Evento = Evento
  { eventoId  :: Int
  , categoria :: Categoria
  , valor     :: Double
  , timestamp :: Int
  } deriving (Show, Eq)


-- ===== Manejo de eventos =====

-- Convierte un indice 0-4 a Categoria 
intACategoria :: Int -> Categoria
intACategoria 0 = Visualizacion
intACategoria 1 = Apartado
intACategoria 2 = Compra
intACategoria 3 = Devolucion
intACategoria _ = Seguimiento

addDaysSimple :: Int -> Day -> Day
addDaysSimple n d = toEnum (fromEnum d + n)

-- Convierte año, mes, dia a YYYYMMDD
fromGregorian2Int :: (Integer, Int, Int) -> Int
fromGregorian2Int (y, m, d) = fromIntegral y * 10000 + m * 100 + d

-- Genera un solo evento aleatorio con un ID dado
generarEvento :: Int -> IO Evento
generarEvento eid = do
  catIdx   <- randomRIO (0, 4 :: Int)
  val      <- randomRIO (500.0, 75000.0 :: Double)
  hoy      <- getCurrentTime
  let diaHoy = utctDay hoy
  diasExtra <- randomRIO (0, 730 :: Int)
  let diaFuturo = toGregorian (addDaysSimple diasExtra diaHoy)
  let ts       = fromGregorian2Int diaFuturo
  valFinal <- if val <= 0
              then do
                putStrLn $ "  [INCONSISTENCIA] Evento " ++ show eid ++
                           " tenia monto invalido (" ++ show val ++
                           "), corregido a 500.0"
                return 500.0
              else return val
  return Evento
    { eventoId  = eid
    , categoria = intACategoria catIdx
    , valor     = valFinal
    , timestamp = ts
    }

-- Genera entre 10 y 25 eventos y los agrega a la lista existente
generarEventos :: [Evento] -> IO [Evento]
generarEventos existentes = do
  cantidad <- randomRIO (10, 25 :: Int)
  let siguienteId = if null existentes
                    then 0
                    else maximum (map eventoId existentes) + 1
  nuevos <- mapM generarEvento [siguienteId .. siguienteId + cantidad - 1]
  putStrLn $ "  [Se generaron " ++ show cantidad ++ " nuevos eventos]"
  return (existentes ++ nuevos)

mostrarEvento :: Evento -> IO ()
mostrarEvento e = putStrLn $
  "  ID: "    ++ show (eventoId e)  ++
  " | Cat: "  ++ show (categoria e) ++
  " | Valor: " ++ show (valor e)    ++
  " | TS: "   ++ show (timestamp e)

-- Transformacion de eventos

aplicarImpuesto :: Evento -> Evento
aplicarImpuesto e
  | categoria e == Compra = e { valor = valor e * 1.13 }
  | otherwise             = e

transformarImpuestos :: [Evento] -> [Evento]
transformarImpuestos = map aplicarImpuesto

promedioValor :: [Evento] -> Double
promedioValor [] = 0
promedioValor es =
  let total    = foldl (\acc e -> acc + valor e) 0 es
      cantidad = fromIntegral (length es)
  in total / cantidad

eventosPorCategoria :: Categoria -> [Evento] -> [Evento]
eventosPorCategoria cat = filter (\e -> categoria e == cat)

esAltoValor :: Double -> Evento -> Bool
esAltoValor promedio e = valor e > promedio

etiquetarAltoValor :: [Evento] -> [(Evento, Bool)]
etiquetarAltoValor eventos =
  let categorias = [Visualizacion, Apartado, Compra, Devolucion, Seguimiento]
      promedios  = map (\cat ->
                     let evsCat = eventosPorCategoria cat eventos
                     in (cat, promedioValor evsCat)
                   ) categorias
      etiquetar e =
        let promCat = case filter (\(c,_) -> c == categoria e) promedios of
                        ((_, p):_) -> p
                        []         -> 0
        in (e, esAltoValor promCat e)
  in map etiquetar eventos

mostrarEtiquetados :: [(Evento, Bool)] -> IO ()
mostrarEtiquetados = mapM_ mostrar
  where
    mostrar (e, alto) = putStrLn $
      "  ID: " ++ show (eventoId e) ++
      " | Cat: " ++ show (categoria e) ++
      " | Valor: " ++ show (valor e) ++
      if alto then " [*** ALTO VALOR ***]" else ""

-- Analisis de datos

montoTotal :: [Evento] -> Double
montoTotal = foldl (\acc e -> acc + valor e) 0

anioDeTimestamp :: Int -> Int
anioDeTimestamp ts = ts `div` 10000

promedioPorCategoriaAnio :: [Evento] -> [(Categoria, Int, Double)]
promedioPorCategoriaAnio eventos =
  let categorias = [Visualizacion, Apartado, Compra, Devolucion, Seguimiento]
      anios      = [2025, 2026, 2027]  
      resultado  = [ (cat, anio, promedioValor evsFiltrados)
                   | cat  <- categorias
                   , anio <- anios
                   , let evsFiltrados = filter
                           (\e -> categoria e == cat &&
                                  anioDeTimestamp (timestamp e) == anio)
                           eventos
                   , not (null evsFiltrados)  -- omite combinaciones vacías
                   ]
  in resultado


mostrarAnalisis :: [Evento] -> IO ()
mostrarAnalisis eventos = do
  putStrLn $ "\n  Monto total: " ++ show (montoTotal eventos)
  putStrLn "\n  Promedio por categoria y anio:"
  let promedios = promedioPorCategoriaAnio eventos
  mapM_ (\(cat, anio, prom) ->
    putStrLn $ "    " ++ show cat ++
               " | " ++ show anio ++
               " | Promedio: " ++ show prom
    ) promedios

-- ANÁLISIS TEMPORAL

mesDeTimestamp :: Int -> Int
mesDeTimestamp ts = (ts `mod` 10000) `div` 100

diaDeTimestamp :: Int -> Int
diaDeTimestamp ts = ts `mod` 100

timestampADay :: Int -> Day
timestampADay ts =
  let anio = fromIntegral (anioDeTimestamp ts)
      mes  = mesDeTimestamp ts
      dia  = diaDeTimestamp ts
  in fromGregorian anio mes dia

mesMayorMonto :: [Evento] -> (Int, Double)
mesMayorMonto [] = (0, 0)
mesMayorMonto eventos =
  let meses     = [1..12]
      totales   = map (\m ->
                    let evsMes = filter (\e -> mesDeTimestamp (timestamp e) == m) eventos
                        total  = foldl (\acc e -> acc + valor e) 0 evsMes
                    in (m, total)
                  ) meses
      ganador   = foldl1 (\(bm, bt) (m, t) -> if t > bt then (m, t) else (bm, bt))
                              totales
  in ganador

diaSemanaActivo :: [Evento] -> String
diaSemanaActivo [] = "Sin datos"
diaSemanaActivo eventos =
  let 
      diasSemana = ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]
      contarDia d = length $ filter (\e ->
                      show (dayOfWeek (timestampADay (timestamp e))) == d
                    ) eventos
      conteos   = map (\d -> (d, contarDia d)) diasSemana
      ganador   = foldl1 (\(bd, bc) (d, c) -> if c > bc then (d, c) else (bd, bc))
                              conteos
  in fst ganador ++ " (" ++ show (snd ganador) ++ " eventos)"

eventoMasAntiguo :: [Evento] -> Evento
eventoMasAntiguo = foldl1 (\acc e -> if timestamp e < timestamp acc then e else acc)

eventoMasReciente :: [Evento] -> Evento
eventoMasReciente = foldl1 (\acc e -> if timestamp e > timestamp acc then e else acc)

resumenPorIntervalo :: [Evento] -> IO ()
resumenPorIntervalo eventos = do
  let intervalos = [ ("  500  - 10000 ", \v -> v >= 500    && v <= 10000)
                   , ("10001  - 30000 ", \v -> v > 10000   && v <= 30000)
                   , ("30001  - 75000 ", \v -> v > 30000   && v <= 75000)
                   ]
  putStrLn "\n  Resumen por intervalo de monto:"
  mapM_ (\(nombre, cond) -> do
    let evs    = filter (cond . valor) eventos
        total  = foldl (\acc e -> acc + valor e) 0 evs
        cant   = length evs
    putStrLn $ "    [" ++ nombre ++ "]" ++
               "  Cantidad: " ++ show cant ++
               "  |  Monto total: " ++ show total
    ) intervalos

mostrarAnalisisTemporal :: [Evento] -> IO ()
mostrarAnalisisTemporal [] = putStrLn "\n  No hay eventos para analizar."
mostrarAnalisisTemporal eventos = do
  let (mes, montoMes) = mesMayorMonto eventos
  putStrLn $ "\n  Mes con mayor monto: " ++ show mes ++
             " | Total: " ++ show montoMes

  putStrLn $ "  Dia de semana mas activo: " ++ diaSemanaActivo eventos

  let antiguo  = eventoMasAntiguo eventos
      reciente = eventoMasReciente eventos
  putStrLn "\n  Evento mas antiguo:"
  mostrarEvento antiguo
  putStrLn "  Evento mas reciente:"
  mostrarEvento reciente

  resumenPorIntervalo eventos

-- ========================= Menu Principal ===================================

mostrarMenu :: IO ()
mostrarMenu = do
  putStrLn "\n=============================="
  putStrLn "  Sistema de Eventos PP2"
  putStrLn "=============================="
  putStrLn "1. Transformacion de eventos"
  putStrLn "2. Analisis de datos"
  putStrLn "3. Analisis temporal"
  putStrLn "4. Busqueda"
  putStrLn "5. Estadisticas"
  putStrLn "0. Salir"
  putStrLn "==============================\n"
  putStr "Seleccione una opcion: "

manejarOpcion :: [Evento] -> String -> IO ()
manejarOpcion eventos "1" = do
  putStrLn "\n--- Transformacion de eventos ---"
  putStrLn "a. Aplicar impuesto a compras"
  putStrLn "b. Etiquetar eventos de alto valor"
  putStr "Seleccione: "
  sub <- getLine
  case sub of
    "a" -> do
      let compras   = filter (\e -> categoria e == Compra) eventos
          transformados = transformarImpuestos eventos
          comprasPost   = filter (\e -> categoria e == Compra) transformados
      putStrLn $ "\nCompras encontradas: " ++ show (length compras)
      putStrLn "\n  ANTES del impuesto:"
      mapM_ mostrarEvento compras
      putStrLn "\n  DESPUES del impuesto (13%):"
      mapM_ mostrarEvento comprasPost
      eventosNuevos <- generarEventos transformados
      menuLoop eventosNuevos
    "b" -> do
      putStrLn "\nEventos etiquetados por alto valor:"
      mostrarEtiquetados (etiquetarAltoValor eventos)
      eventosNuevos <- generarEventos eventos
      menuLoop eventosNuevos
    _ -> do
      putStrLn "\nOpcion no valida."
      eventosNuevos <- generarEventos eventos
      menuLoop eventosNuevos

manejarOpcion eventos "2" = do
  putStrLn "\n--- Analisis de datos ---"
  mostrarAnalisis eventos
  eventosNuevos <- generarEventos eventos
  menuLoop eventosNuevos

manejarOpcion eventos "3" = do
  putStrLn "\n--- Analisis Temporal ---"
  mostrarAnalisisTemporal eventos
  eventosNuevos <- generarEventos eventos
  menuLoop eventosNuevos

manejarOpcion eventos "4" = do
  putStrLn "\n[Busqueda - por implementar]"
  eventosNuevos <- generarEventos eventos
  menuLoop eventosNuevos

manejarOpcion eventos "5" = do
  putStrLn "\n[Estadisticas - mostrando eventos actuales]"
  putStrLn $ "Total de eventos: " ++ show (length eventos)
  mapM_ mostrarEvento (take 5 eventos)
  eventosNuevos <- generarEventos eventos
  menuLoop eventosNuevos

manejarOpcion _ "0" = putStrLn "\nHasta luego!\n"

manejarOpcion eventos _ = do
  putStrLn "\nOpcion no valida. Intente de nuevo."
  menuLoop eventos

menuLoop :: [Evento] -> IO ()
menuLoop eventos = do
  mostrarMenu
  opcion <- getLine
  manejarOpcion eventos opcion

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Iniciando sistema..."
  eventosIniciales <- generarEventos []
  menuLoop eventosIniciales