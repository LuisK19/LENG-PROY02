module Main (main) where

import System.IO     (hSetBuffering, stdout, BufferMode(..))
import System.Random (randomRIO)
import Data.Time (getCurrentTime, utctDay, toGregorian,
                  fromGregorian, Day, dayOfWeek, DayOfWeek(..))

import Data.List        (intercalate, sortBy, groupBy, maximumBy)
import Data.Ord         (comparing, Down(..))
import Data.Function    (on)
import System.Directory (createDirectoryIfMissing)

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

-- Analisis temporal

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


-- Busqueda por rango de fechas

buscarPorRango :: Int -> Int -> [Evento] -> [Evento]
buscarPorRango inicio fin = filter (\e -> timestamp e >= inicio && timestamp e <= fin)

ejecutarBusqueda :: [Evento] -> IO ()
ejecutarBusqueda eventos = do
  putStrLn "\n  Ingrese fecha inicio (YYYYMMDD, ej: 20260101):"
  putStr "  > "
  inputInicio <- getLine
  putStrLn "  Ingrese fecha fin (YYYYMMDD, ej: 20271231):"
  putStr "  > "
  inputFin <- getLine

  case (reads inputInicio, reads inputFin) of
    ([(inicio, "")], [(fin, "")]) -> do
      if inicio > fin
        then putStrLn "\n  [ERROR] La fecha inicio no puede ser mayor que la fecha fin."
        else do
          let resultados = buscarPorRango inicio fin eventos
          putStrLn $ "\n  Eventos encontrados: " ++ show (length resultados)
          if null resultados
            then putStrLn "  No se encontraron eventos en ese rango."
            else mapM_ mostrarEvento resultados
    _ -> putStrLn "\n  [ERROR] Formato de fecha invalido. Use YYYYMMDD."

-- Estadisticas

cantidadPorCategoria :: [Evento] -> [(Categoria, Int)]
cantidadPorCategoria eventos =
  let cats = [Visualizacion, Apartado, Compra, Devolucion, Seguimiento]
  in map (\c -> (c, length $ filter (\e -> categoria e == c) eventos)) cats

eventoMayor :: [Evento] -> Evento
eventoMayor = maximumBy (comparing valor)

eventoMenor :: [Evento] -> Evento
eventoMenor = maximumBy (comparing (Down . valor))

diaMayorEventos :: [Evento] -> (Int, Int)
diaMayorEventos [] = (0, 0)
diaMayorEventos eventos =
  let agrupados = map (\e -> timestamp e) eventos
      contarDia d = (d, length $ filter (\e -> timestamp e == d) eventos)
      conteos   = map contarDia agrupados
  in foldl1 (\(bd, bc) (d, c) -> if c > bc then (d, c) else (bd, bc)) conteos

mostrarEstadisticas :: [Evento] -> IO ()
mostrarEstadisticas [] = putStrLn "\n  No hay eventos para mostrar."
mostrarEstadisticas eventos = do
  putStrLn "\n  --- Cantidad por categoria ---"
  let cats = cantidadPorCategoria eventos
  mapM_ (\(c, n) -> putStrLn $ "    " ++ show c ++ ": " ++ show n) cats

  putStrLn "\n  --- Evento con monto mas alto ---"
  mostrarEvento (eventoMayor eventos)

  putStrLn "\n  --- Evento con monto mas bajo ---"
  mostrarEvento (eventoMenor eventos)

  let (dia, cant) = diaMayorEventos eventos
  putStrLn $ "\n  --- Dia con mayor cantidad de eventos ---"
  putStrLn $ "    Fecha: " ++ show dia ++ " | Eventos: " ++ show cant

-- CSV

eventoACSV :: Evento -> String
eventoACSV e = intercalate ","
  [ show (eventoId e)
  , show (categoria e)
  , show (valor e)
  , show (timestamp e)
  ]

-- Exporta las estadísticas calculadas a CSV
exportarEstadisticasCSV :: [Evento] -> IO ()
exportarEstadisticasCSV eventos = do
  createDirectoryIfMissing True "exports"
  let cats    = cantidadPorCategoria eventos
      mayor   = eventoMayor eventos
      menor   = eventoMenor eventos
      (dia, cantDia) = diaMayorEventos eventos
      -- Sección 1: cantidad por categoría
      headerCats  = "categoria,cantidad"
      lineasCats  = map (\(c,n) -> categoriaAString c ++ "," ++ show n) cats
      -- Sección 2: eventos extremos
      headerExtr  = "tipo,id,categoria,valor,timestamp"
      lineaMayor  = "mayor," ++ show (eventoId mayor) ++ "," ++
                    categoriaAString (categoria mayor) ++ "," ++
                    show (valor mayor) ++ "," ++ show (timestamp mayor)
      lineaMenor  = "menor," ++ show (eventoId menor) ++ "," ++
                    categoriaAString (categoria menor) ++ "," ++
                    show (valor menor) ++ "," ++ show (timestamp menor)
      -- Sección 3: día más activo
      headerDia   = "fecha,cantidad_eventos"
      lineaDia    = show dia ++ "," ++ show cantDia
      -- Armar contenido completo
      contenido   = unlines $
                    ["# Cantidad por categoria", headerCats] ++ lineasCats ++
                    ["", "# Eventos extremos", headerExtr, lineaMayor, lineaMenor] ++
                    ["", "# Dia con mayor eventos", headerDia, lineaDia]
  writeFile "exports/estadisticas.csv" contenido
  putStrLn "  [OK] Estadisticas exportadas a exports/estadisticas.csv"

-- Exporta las estadísticas calculadas a JSON
exportarEstadisticasJSON :: [Evento] -> IO ()
exportarEstadisticasJSON eventos = do
  createDirectoryIfMissing True "exports"
  let cats   = cantidadPorCategoria eventos
      mayor  = eventoMayor eventos
      menor  = eventoMenor eventos
      (dia, cantDia) = diaMayorEventos eventos
      -- Sección categorías
      jsonCats = "  \"cantidad_por_categoria\": {\n" ++
                 intercalate ",\n"
                   (map (\(c,n) -> "    \"" ++ categoriaAString c ++
                                   "\": " ++ show n) cats) ++
                 "\n  }"
      -- Sección extremos
      jsonMayor = "    {\"tipo\":\"mayor\",\"id\":" ++ show (eventoId mayor) ++
                  ",\"categoria\":\"" ++ categoriaAString (categoria mayor) ++
                  "\",\"valor\":" ++ show (valor mayor) ++
                  ",\"timestamp\":" ++ show (timestamp mayor) ++ "}"
      jsonMenor = "    {\"tipo\":\"menor\",\"id\":" ++ show (eventoId menor) ++
                  ",\"categoria\":\"" ++ categoriaAString (categoria menor) ++
                  "\",\"valor\":" ++ show (valor menor) ++
                  ",\"timestamp\":" ++ show (timestamp menor) ++ "}"
      jsonExtr  = "  \"eventos_extremos\": [\n" ++
                  intercalate ",\n" [jsonMayor, jsonMenor] ++
                  "\n  ]"
      -- Sección día más activo
      jsonDia   = "  \"dia_mayor_eventos\": {\n" ++
                  "    \"fecha\": " ++ show dia ++ ",\n" ++
                  "    \"cantidad\": " ++ show cantDia ++ "\n" ++
                  "  }"
      contenido = "{\n" ++ intercalate ",\n" [jsonCats, jsonExtr, jsonDia] ++ "\n}"
  writeFile "exports/estadisticas.json" contenido
  putStrLn "  [OK] Estadisticas exportadas a exports/estadisticas.json"

-- JSON

categoriaAString :: Categoria -> String
categoriaAString Visualizacion = "visualizacion"
categoriaAString Apartado      = "apartado"
categoriaAString Compra        = "compra"
categoriaAString Devolucion    = "devolucion"
categoriaAString Seguimiento   = "seguimiento"

eventoAJSON :: Evento -> String
eventoAJSON e = concat
  [ "  {"
  , "\"id\":"        , show (eventoId e)  , ","
  , "\"categoria\":" , "\"" ++ categoriaAString (categoria e) ++ "\","
  , "\"valor\":"     , show (valor e)     , ","
  , "\"timestamp\":" , show (timestamp e)
  , "}"
  ]

exportarJSON :: [Evento] -> IO ()
exportarJSON eventos = do
  createDirectoryIfMissing True "exports"
  let objetos   = map eventoAJSON eventos
      contenido = "[\n" ++ intercalate ",\n" objetos ++ "\n]"
  writeFile "exports/estadisticas.json" contenido
  putStrLn "  [OK] Exportado a exports/estadisticas.json"

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
  putStrLn "\n--- Busqueda por rango de fechas ---"
  ejecutarBusqueda eventos
  eventosNuevos <- generarEventos eventos
  menuLoop eventosNuevos

manejarOpcion eventos "5" = do
  putStrLn "\n--- Estadisticas ---"
  mostrarEstadisticas eventos
  putStrLn "\n  Exportar reporte:"
  putStrLn "  a. CSV"
  putStrLn "  b. JSON"
  putStrLn "  c. Ambos"
  putStrLn "  d. No exportar"
  putStr "  Seleccione: "
  sub <- getLine
  case sub of
    "a" -> exportarEstadisticasCSV eventos
    "b" -> exportarEstadisticasJSON eventos
    "c" -> do
      exportarEstadisticasCSV eventos
      exportarEstadisticasJSON eventos
    _   -> putStrLn "  [Sin exportar]"
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