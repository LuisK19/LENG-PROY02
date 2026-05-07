-- Proyecto Programado #2 - Sistema de Eventos Comerciales
-- Curso: Lenguajes de Programación
-- Semestre I, 2026

module Main (main) where

import System.IO     (hSetBuffering, stdout, BufferMode(..))
import System.Random (randomRIO)
import Data.Time     (getCurrentTime, utctDay, utctDayTime, toGregorian,
                      fromGregorian, Day, dayOfWeek, UTCTime (utctDayTime))
import Data.List     (intercalate, maximumBy)
import Data.Ord      (comparing, Down(..))
import System.Directory (createDirectoryIfMissing)


-- ======== TIPOS DE DATOS ===================================================================

-- | Tipo suma que representa las categorías posibles de un evento.
--   Cada constructor corresponde a un tipo de acción dentro de la plataforma.
data Categoria
  = Visualizacion
  | Apartado
  | Compra
  | Devolucion
  | Seguimiento
  deriving (Show, Eq, Ord)

-- | Tipo producto que representa un evento de la plataforma.
--   Campos:
--     eventoId  - Identificador único entre 0 y 9,000,000
--     categoria - Tipo de acción realizada
--     valor     - Monto asociado al evento (mayor a cero)
--     timestamp - Fecha del evento en formato YYYYMMDD
data Evento = Evento
  { eventoId  :: Int
  , categoria :: Categoria
  , valor     :: Double
  , timestamp :: Int
  } deriving (Show, Eq)

-- ======== UTILIDADES DE CONVERSIÓN ===================================================================

-- | Convierte un índice entero (0-4) al constructor de Categoria correspondiente.
--   Entrada:  Int entre 0 y 4
--   Salida:   Categoria correspondiente (cualquier valor >= 4 retorna Seguimiento)
--   Restricción: no lanza error para valores fuera de rango gracias al caso por defecto
intACategoria :: Int -> Categoria
intACategoria 0 = Visualizacion
intACategoria 1 = Apartado
intACategoria 2 = Compra
intACategoria 3 = Devolucion
intACategoria _ = Seguimiento

-- | Suma una cantidad de días a un valor de tipo Day.
--   Entrada:  n - cantidad de días a sumar; d - fecha base
--   Salida:   nueva fecha con n días adicionales
--   Restricción: n debe ser positivo para avanzar en el tiempo
addDaysSimple :: Int -> Day -> Day
addDaysSimple n d = toEnum (fromEnum d + n)

-- | Convierte una tupla (año, mes, día) a un entero en formato YYYYMMDD.
--   Entrada:  tupla (Integer, Int, Int) con año, mes y día
--   Salida:   entero que representa la fecha, ej: (2026, 5, 3) -> 20260503
fromGregorian2Int :: (Integer, Int, Int) -> Int
fromGregorian2Int (y, m, d) = fromIntegral y * 10000 + m * 100 + d

-- | Extrae el año de un timestamp en formato YYYYMMDD.
--   Entrada:  entero YYYYMMDD
--   Salida:   año como Int, ej: 20261215 -> 2026
anioDeTimestamp :: Int -> Int
anioDeTimestamp ts = ts `div` 10000

-- | Extrae el mes de un timestamp en formato YYYYMMDD.
--   Entrada:  entero YYYYMMDD
--   Salida:   mes como Int (1-12), ej: 20261215 -> 12
mesDeTimestamp :: Int -> Int
mesDeTimestamp ts = (ts `mod` 10000) `div` 100

-- | Extrae el día de un timestamp en formato YYYYMMDD.
--   Entrada:  entero YYYYMMDD
--   Salida:   día como Int (1-31), ej: 20261215 -> 15
diaDeTimestamp :: Int -> Int
diaDeTimestamp ts = ts `mod` 100

-- | Convierte un timestamp YYYYMMDD al tipo Day de Data.Time.
--   Necesario para calcular el día de la semana con dayOfWeek.
--   Entrada:  entero YYYYMMDD
--   Salida:   valor de tipo Day
timestampADay :: Int -> Day
timestampADay ts =
  let anio = fromIntegral (anioDeTimestamp ts)
      mes  = mesDeTimestamp ts
      dia  = diaDeTimestamp ts
  in fromGregorian anio mes dia

-- | Convierte una Categoria a su representación en String minúscula.
--   Usado para exportar datos a CSV y JSON.
--   Entrada:  valor de tipo Categoria
--   Salida:   String con el nombre en minúscula
categoriaAString :: Categoria -> String
categoriaAString Visualizacion = "visualizacion"
categoriaAString Apartado      = "apartado"
categoriaAString Compra        = "compra"
categoriaAString Devolucion    = "devolucion"
categoriaAString Seguimiento   = "seguimiento"

--  ======== GENERADOR DINÁMICO DE EVENTOS ========================================================

-- | Genera un único evento con todos sus campos aleatorios.
--   El ID es aleatorio entre 0 y 9,000,000.
--   El valor es aleatorio entre 500.0 y 75,000.0.
--   El timestamp va desde el día actual hasta 2 años en el futuro.
--   Valida que el monto sea mayor a cero y reporta inconsistencias.
--   Salida: IO Evento con datos generados aleatoriamente
generarEvento :: IO Evento
generarEvento = do
  eid      <- randomRIO (0, 9000000 :: Int)
  catIdx   <- randomRIO (0, 4 :: Int)
  val      <- randomRIO (500.0, 75000.0 :: Double)
  hoy      <- getCurrentTime
  let diaHoy = utctDay hoy
  diasExtra <- randomRIO (0, 730 :: Int)
  let diaFuturo = toGregorian (addDaysSimple diasExtra diaHoy)
  let ts        = fromGregorian2Int diaFuturo
  -- Validación: monto debe ser mayor a cero; se reporta y corrige si no lo es
  valFinal <- if val <= 0
              then do
                putStrLn $ "  [INCONSISTENCIA] Evento " ++ show eid ++
                           " tenia monto invalido, corregido a 500.0"
                return 500.0
              else return val
  return Evento
    { eventoId  = eid
    , categoria = intACategoria catIdx
    , valor     = valFinal
    , timestamp = ts
    }

-- | Genera entre 10 y 25 eventos nuevos y los agrega a la lista existente.
--   Se llama automáticamente al finalizar cada opción del menú.
--   Entrada:  lista de eventos existentes
--   Salida:   IO con la lista original más los nuevos eventos concatenados
generarEventos :: [Evento] -> IO [Evento]
generarEventos existentes = do
  cantidad <- randomRIO (10, 25 :: Int)
  nuevos   <- mapM (const generarEvento) [1..cantidad]
  putStrLn $ "  [Se generaron " ++ show cantidad ++ " nuevos eventos]"
  return (existentes ++ nuevos)

-- | Imprime un evento en consola con formato legible.
--   Entrada:  Evento a mostrar
--   Salida:   IO () - imprime ID, categoría, valor y timestamp
mostrarEvento :: Evento -> IO ()
mostrarEvento e = putStrLn $
  "  ID: "     ++ show (eventoId e)  ++
  " | Cat: "   ++ show (categoria e) ++
  " | Valor: " ++ show (valor e)     ++
  " | TS: "    ++ show (timestamp e)

-- ======== TRANSFORMACIÓN DE EVENTOS ===================================================================

-- | Aplica el impuesto de ventas del 13% al valor de un evento de tipo Compra.
--   Si el evento no es de categoría Compra, lo retorna sin modificaciones.
--   Entrada:  Evento original
--   Salida:   Evento con valor multiplicado por 1.13 si es Compra, sin cambios si no
--   Nota: usa record update syntax para no mutar el evento sino crear uno nuevo
aplicarImpuesto :: Evento -> Evento
aplicarImpuesto e
  | categoria e == Compra = e { valor = valor e * 1.13 }
  | otherwise             = e

-- | Aplica el impuesto del 13% a todos los eventos de la lista usando map.
--   Solo los eventos de categoría Compra serán modificados.
--   Entrada:  lista de eventos
--   Salida:   nueva lista con compras actualizadas y el resto sin cambios
transformarImpuestos :: [Evento] -> [Evento]
transformarImpuestos = map aplicarImpuesto

-- | Calcula el promedio del campo valor de una lista de eventos.
--   Entrada:  lista de eventos (puede estar vacía)
--   Salida:   Double con el promedio; retorna 0 si la lista está vacía
--   Restricción: maneja lista vacía explícitamente para evitar división por cero
promedioValor :: [Evento] -> Double
promedioValor [] = 0
promedioValor es =
  let total    = foldl (\acc e -> acc + valor e) 0 es
      cantidad = fromIntegral (length es)
  in total / cantidad

-- | Filtra los eventos de una lista que pertenecen a una categoría específica.
--   Entrada:  cat - categoría a filtrar; lista de eventos
--   Salida:   sublista con solo los eventos de esa categoría
eventosPorCategoria :: Categoria -> [Evento] -> [Evento]
eventosPorCategoria cat = filter (\e -> categoria e == cat)

-- | Determina si un evento es de alto valor comparándolo con un promedio dado.
--   Entrada:  promedio - valor de referencia; e - evento a evaluar
--   Salida:   True si el valor del evento supera el promedio, False si no
esAltoValor :: Double -> Evento -> Bool
esAltoValor promedio e = valor e > promedio

-- | Etiqueta cada evento indicando si es de alto valor respecto al promedio
--   de su propia categoría. El promedio se recalcula en cada llamada.
--   Entrada:  lista de eventos
--   Salida:   lista de tuplas (Evento, Bool) donde Bool indica si es alto valor
--   Nota: usa composición de map y filter para calcular promedios por categoría
etiquetarAltoValor :: [Evento] -> [(Evento, Bool)]
etiquetarAltoValor eventos =
  let categorias = [Visualizacion, Apartado, Compra, Devolucion, Seguimiento]
      -- Para cada categoría calcula su promedio y lo asocia en una tupla
      promedios  = map (\cat ->
                     let evsCat = eventosPorCategoria cat eventos
                     in (cat, promedioValor evsCat)
                   ) categorias
      -- Para cada evento busca el promedio de su categoría y evalúa si es alto valor
      etiquetar e =
        let promCat = case filter (\(c,_) -> c == categoria e) promedios of
                        ((_, p):_) -> p
                        []         -> 0
        in (e, esAltoValor promCat e)
  in map etiquetar eventos

-- | Imprime la lista de eventos etiquetados con su indicador de alto valor.
--   Entrada:  lista de tuplas (Evento, Bool)
--   Salida:   IO () - imprime cada evento con [*** ALTO VALOR ***] si aplica
mostrarEtiquetados :: [(Evento, Bool)] -> IO ()
mostrarEtiquetados = mapM_ mostrar
  where
    mostrar (e, alto) = putStrLn $
      "  ID: " ++ show (eventoId e) ++
      " | Cat: " ++ show (categoria e) ++
      " | Valor: " ++ show (valor e) ++
      if alto then " [*** ALTO VALOR ***]" else ""

-- ======== ANÁLISIS DE DATOS ===================================================================

-- | Calcula la suma de los valores de todos los eventos usando foldl.
--   Entrada:  lista de eventos
--   Salida:   Double con la suma total de todos los montos
montoTotal :: [Evento] -> Double
montoTotal = foldl (\acc e -> acc + valor e) 0

-- | Calcula el promedio de valor agrupado por categoría y año.
--   Usa list comprehension para generar todas las combinaciones válidas.
--   Entrada:  lista de eventos
--   Salida:   lista de tuplas (Categoria, Int, Double) con cat, año y promedio
--   Restricción: omite combinaciones donde no hay eventos ese año/categoría
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
                   , not (null evsFiltrados)
                   ]
  in resultado

-- ======== ANÁLISIS TEMPORAL ===================================================================

-- | Encuentra el mes y año con mayor monto total acumulado.
--   Separa por año para que enero 2026 y enero 2027 no se mezclen.
--   Entrada:  lista de eventos
--   Salida:   tupla (año, mes, monto) del período con mayor acumulado
--   Restricción: retorna (0,0,0) si la lista está vacía
mesMayorMonto :: [Evento] -> (Int, Int, Double)
mesMayorMonto [] = (0, 0, 0)
mesMayorMonto eventos =
  let anios   = [2025, 2026, 2027, 2028]
      meses   = [1..12]
      -- List comprehension: genera tuplas (año, mes, total) para cada período con eventos
      totales = [ (anio, mes, total)
                | anio <- anios
                , mes  <- meses
                , let evs = filter (\e -> anioDeTimestamp (timestamp e) == anio
                                       && mesDeTimestamp  (timestamp e) == mes) eventos
                , not (null evs)
                , let total = foldl (\acc e -> acc + valor e) 0 evs
                ]
  in if null totales then (0, 0, 0)
     else foldl1 (\(ba, bm, bt) (a, m, t) ->
                    if t > bt then (a, m, t) else (ba, bm, bt)) totales

-- | Determina el día de la semana con más eventos registrados.
--   Convierte cada timestamp a Day y usa dayOfWeek para obtener el día.
--   Entrada:  lista de eventos
--   Salida:   String con el nombre del día y la cantidad de eventos
diaSemanaActivo :: [Evento] -> String
diaSemanaActivo [] = "Sin datos"
diaSemanaActivo eventos =
  let diasSemana = ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]
      -- Cuenta cuántos eventos caen en cada día de la semana
      contarDia d = length $ filter (\e ->
                      show (dayOfWeek (timestampADay (timestamp e))) == d
                    ) eventos
      conteos  = map (\d -> (d, contarDia d)) diasSemana
      ganador  = foldl1 (\(bd, bc) (d, c) -> if c > bc then (d, c) else (bd, bc)) conteos
  in fst ganador ++ " (" ++ show (snd ganador) ++ " eventos)"

-- | Retorna el evento con el timestamp más antiguo (menor valor).
--   Entrada:  lista de eventos (no vacía)
--   Salida:   Evento con el timestamp menor
--   Restricción: falla con lista vacía; debe validarse antes de llamar
eventoMasAntiguo :: [Evento] -> Evento
eventoMasAntiguo = foldl1 (\acc e -> if timestamp e < timestamp acc then e else acc)

-- | Retorna el evento con el timestamp más reciente (mayor valor).
--   Entrada:  lista de eventos (no vacía)
--   Salida:   Evento con el timestamp mayor
--   Restricción: falla con lista vacía; debe validarse antes de llamar
eventoMasReciente :: [Evento] -> Evento
eventoMasReciente = foldl1 (\acc e -> if timestamp e > timestamp acc then e else acc)

-- | Muestra un resumen de cantidad y monto total agrupado por intervalos de valor.
--   Intervalos: 500-10000, 10001-30000, 30001-75000.
--   Entrada:  lista de eventos
--   Salida:   IO () - imprime cada intervalo con su cantidad y monto total
resumenPorIntervalo :: [Evento] -> IO ()
resumenPorIntervalo eventos = do
  let intervalos = [ ("  500  - 10000 ", \v -> v >= 500  && v <= 10000)
                   , ("10001  - 30000 ", \v -> v > 10000 && v <= 30000)
                   , ("30001  - 75000 ", \v -> v > 30000 && v <= 75000)
                   ]
  putStrLn "\n  Resumen por intervalo de monto:"
  mapM_ (\(nombre, cond) -> do
    let evs   = filter (cond . valor) eventos
        total = foldl (\acc e -> acc + valor e) 0 evs
        cant  = length evs
    putStrLn $ "    [" ++ nombre ++ "]" ++
               "  Cantidad: " ++ show cant ++
               "  |  Monto total: " ++ show total
    ) intervalos

-- ======== BÚSQUEDA ===================================================================

-- | Filtra eventos cuyo timestamp esté dentro del rango [inicio, fin].
--   Entrada:  inicio - fecha mínima YYYYMMDD; fin - fecha máxima YYYYMMDD; lista
--   Salida:   sublista con eventos dentro del rango inclusive
buscarPorRango :: Int -> Int -> [Evento] -> [Evento]
buscarPorRango inicio fin = filter (\e -> timestamp e >= inicio && timestamp e <= fin)

-- | Valida que un timestamp YYYYMMDD sea una fecha real
fechaValida :: Int -> Bool
fechaValida ts =
  let anio = ts `div` 10000
      mes  = (ts `mod` 10000) `div` 100
      dia  = ts `mod` 100
      diasPorMes = [31,28,31,30,31,30,31,31,30,31,30,31]
      esBisiesto = (anio `mod` 4 == 0 && anio `mod` 100 /= 0)
                || anio `mod` 400 == 0
      maxDias m
        | m == 2 && esBisiesto = 29
        | m >= 1 && m <= 12    = diasPorMes !! (m-1)
        | otherwise            = 0
  in anio >= 2020 && anio <= 2035
  && mes >= 1 && mes <= 12
  && dia >= 1 && dia <= maxDias mes

-- | Interactúa con el usuario para obtener fechas y ejecutar la búsqueda.
--   Lee dos fechas en formato YYYYMMDD, valida el formato y el orden lógico.
--   Entrada:  lista de eventos sobre la cual buscar
--   Salida:   IO () - imprime los eventos encontrados o mensajes de error
ejecutarBusqueda :: [Evento] -> IO ()
ejecutarBusqueda eventos = do
  putStrLn "\n  Rango de fechas valido: 20200101 hasta 20351231"
  putStrLn "\n  Ingrese fecha inicio (YYYYMMDD, ej: 20260101):"
  putStr "  > "
  inputInicio <- getLine
  putStrLn "  Ingrese fecha fin (YYYYMMDD, ej: 20271231):"
  putStr "  > "
  inputFin <- getLine
  -- reads parsea el String de forma segura; lista vacía indica formato inválido
  case (reads inputInicio, reads inputFin) of
    ([(inicio, "")], [(fin, "")]) -> do
      if not (fechaValida inicio)
        then putStrLn "\n  ERROR: La fecha inicio no es una fecha valida."
      else if not (fechaValida fin)
        then putStrLn "\n  ERROR: La fecha fin no es una fecha valida."
      else if inicio > fin
        then putStrLn "\n  ERROR: La fecha inicio no puede ser mayor que la fecha fin."
        else do
          let resultados = buscarPorRango inicio fin eventos
          putStrLn $ "\n  Eventos encontrados: " ++ show (length resultados)
          if null resultados
            then putStrLn "  No se encontraron eventos en ese rango."
            else mapM_ mostrarEvento resultados
    _ -> putStrLn "\n  ERROR: Formato invalido. Use YYYYMMDD (ejemplo: 20260101)."

-- ======== ESTADÍSTICAS ===================================================================

-- | Cuenta la cantidad de eventos por cada categoría.
--   Entrada:  lista de eventos
--   Salida:   lista de tuplas (Categoria, Int) con el conteo por categoría
cantidadPorCategoria :: [Evento] -> [(Categoria, Int)]
cantidadPorCategoria eventos =
  let cats = [Visualizacion, Apartado, Compra, Devolucion, Seguimiento]
  in map (\c -> (c, length $ filter (\e -> categoria e == c) eventos)) cats

-- | Retorna el evento con el valor más alto usando maximumBy.
--   Entrada:  lista de eventos (no vacía)
--   Salida:   Evento con el campo valor mayor
eventoMayor :: [Evento] -> Evento
eventoMayor = maximumBy (comparing valor)

-- | Retorna el evento con el valor más bajo usando maximumBy con Down.
--   Down invierte el orden natural para encontrar el mínimo como si fuera máximo.
--   Entrada:  lista de eventos (no vacía)
--   Salida:   Evento con el campo valor menor
eventoMenor :: [Evento] -> Evento
eventoMenor = maximumBy (comparing (Down . valor))

-- | Encuentra la fecha (YYYYMMDD) con mayor cantidad de eventos registrados.
--   Entrada:  lista de eventos
--   Salida:   tupla (fecha, cantidad) del día con más eventos
--   Restricción: retorna (0,0) si la lista está vacía
diaMayorEventos :: [Evento] -> (Int, Int)
diaMayorEventos [] = (0, 0)
diaMayorEventos eventos =
  let agrupados = map timestamp eventos
      contarDia d = (d, length $ filter (\e -> timestamp e == d) eventos)
      conteos   = map contarDia agrupados
  in foldl1 (\(bd, bc) (d, c) -> if c > bc then (d, c) else (bd, bc)) conteos

-- | Muestra el resumen general de estadísticas en consola.
--   Incluye: cantidad por categoría, evento mayor/menor y día más activo.
--   Entrada:  lista de eventos
--   Salida:   IO () - imprime las estadísticas formateadas
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
  putStrLn "\n  --- Dia con mayor cantidad de eventos ---"
  putStrLn $ "    Fecha: " ++ show dia ++ " | Eventos: " ++ show cant

-- ======== EXPORTACIÓN ===================================================================

-- | Exporta las estadísticas calculadas a un archivo CSV con nombre único por fecha.
--   El archivo incluye: cantidad por categoría, eventos extremos y día más activo.
--   Crea la carpeta exports/ si no existe.
--   Entrada:  lista de eventos
--   Salida:   IO () - escribe el archivo y confirma en consola
exportarEstadisticasCSV :: [Evento] -> IO ()
exportarEstadisticasCSV eventos = do
  createDirectoryIfMissing True "exports"
  ahora <- getCurrentTime
  let (y, m, d) = toGregorian (utctDay ahora)
      pad n = if n < 10 then "0" ++ show n else show n
      totalSegs = floor (utctDayTime ahora) :: Int
      hh = totalSegs `div` 3600
      mm = (totalSegs `mod` 3600) `div` 60
      ss = totalSegs `mod` 60
      nombreArchivo = "exports/estadisticas_" ++ show y ++
                      "-" ++ pad m ++ "-" ++ pad d ++ "_" ++ pad hh ++ "-" ++ pad mm ++ "-" ++ pad ss ++ ".csv"
  let cats           = cantidadPorCategoria eventos
      mayor          = eventoMayor eventos
      menor          = eventoMenor eventos
      (dia, cantDia) = diaMayorEventos eventos
      headerCats     = "categoria,cantidad"
      lineasCats     = map (\(c,n) -> categoriaAString c ++ "," ++ show n) cats
      headerExtr     = "tipo,id,categoria,valor,timestamp"
      lineaMayor     = "mayor," ++ show (eventoId mayor) ++ "," ++
                       categoriaAString (categoria mayor) ++ "," ++
                       show (valor mayor) ++ "," ++ show (timestamp mayor)
      lineaMenor     = "menor," ++ show (eventoId menor) ++ "," ++
                       categoriaAString (categoria menor) ++ "," ++
                       show (valor menor) ++ "," ++ show (timestamp menor)
      headerDia      = "fecha,cantidad_eventos"
      lineaDia       = show dia ++ "," ++ show cantDia
      contenido      = unlines $
                       ["# Cantidad por categoria", headerCats] ++ lineasCats ++
                       ["", "# Eventos extremos", headerExtr, lineaMayor, lineaMenor] ++
                       ["", "# Dia con mayor eventos", headerDia, lineaDia]
  writeFile nombreArchivo contenido
  putStrLn $ "  Exitoso: Estadisticas exportadas a " ++ nombreArchivo

-- | Exporta las estadísticas calculadas a un archivo JSON con nombre único por fecha.
--   El archivo incluye: cantidad por categoría, eventos extremos y día más activo.
--   Crea la carpeta exports/ si no existe.
--   Entrada:  lista de eventos
--   Salida:   IO () - escribe el archivo y confirma en consola
exportarEstadisticasJSON :: [Evento] -> IO ()
exportarEstadisticasJSON eventos = do
  createDirectoryIfMissing True "exports"
  ahora <- getCurrentTime
  let (y, m, d) = toGregorian (utctDay ahora)
      pad n = if n < 10 then "0" ++ show n else show n
      totalSegs = floor (utctDayTime ahora) :: Int
      hh = totalSegs `div` 3600
      mm = (totalSegs `mod` 3600) `div` 60
      ss = totalSegs `mod` 60
      nombreArchivo = "exports/estadisticas_" ++ show y ++
                      "-" ++ pad m ++ "-" ++ pad d ++"_" ++ pad hh ++ "-" ++ pad mm ++ "-" ++ pad ss ++ ".json"
  let cats           = cantidadPorCategoria eventos
      mayor          = eventoMayor eventos
      menor          = eventoMenor eventos
      (dia, cantDia) = diaMayorEventos eventos
      jsonCats  = "  \"cantidad_por_categoria\": {\n" ++
                  intercalate ",\n"
                    (map (\(c,n) -> "    \"" ++ categoriaAString c ++
                                    "\": " ++ show n) cats) ++
                  "\n  }"
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
      jsonDia   = "  \"dia_mayor_eventos\": {\n" ++
                  "    \"fecha\": " ++ show dia ++ ",\n" ++
                  "    \"cantidad\": " ++ show cantDia ++ "\n" ++
                  "  }"
      contenido = "{\n" ++ intercalate ",\n" [jsonCats, jsonExtr, jsonDia] ++ "\n}"
  writeFile nombreArchivo contenido
  putStrLn $ "  Exitoso: Estadisticas exportadas a " ++ nombreArchivo

-- ======== MENÚ PRINCIPAL ===================================================================

-- | Imprime el menú principal con todas las opciones disponibles.
--   Salida: IO () - muestra el menú en consola
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

-- | Despacha la acción correspondiente según la opción elegida por el usuario.
--   Al finalizar cada opción (excepto salir) genera nuevos eventos y vuelve al menú.
--   Entrada:  lista de eventos actual; String con la opción ingresada
--   Salida:   IO () - ejecuta la acción y recursa en menuLoop
manejarOpcion :: [Evento] -> String -> IO ()
manejarOpcion eventos "1" = do
  putStrLn "\n--- Transformacion de eventos ---"
  putStrLn "a. Aplicar impuesto a compras"
  putStrLn "b. Etiquetar eventos de alto valor"
  putStr "Seleccione: "
  sub <- getLine
  case sub of
    "a" -> do
      let compras       = filter (\e -> categoria e == Compra) eventos
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
  putStrLn "a. Monto total"
  putStrLn "b. Promedio por categoria y anio"
  putStr "Seleccione: "
  sub <- getLine
  case sub of
    "a" -> putStrLn $ "\n  Monto total: " ++ show (montoTotal eventos)
    "b" -> do
      putStrLn "\n  Promedio por categoria y anio:"
      let promedios = promedioPorCategoriaAnio eventos
      mapM_ (\(cat, anio, prom) ->
        putStrLn $ "    " ++ show cat ++
                   " | " ++ show anio ++
                   " | Promedio: " ++ show prom
        ) promedios
    _ -> putStrLn "\n  Opcion no valida."
  eventosNuevos <- generarEventos eventos
  menuLoop eventosNuevos

manejarOpcion eventos "3" = do
  putStrLn "\n--- Analisis Temporal ---"
  putStrLn "a. Mes con mayor monto y dia mas activo"
  putStrLn "b. Evento mas antiguo y reciente"
  putStrLn "c. Resumen por intervalo"
  putStr "Seleccione: "
  sub <- getLine
  case sub of
    "a" -> do
      let (anio, mes, montoMes) = mesMayorMonto eventos
      putStrLn $ "\n  Mes con mayor monto: " ++ show mes ++
                 "/" ++ show anio ++
                 " | Total: " ++ show montoMes
      putStrLn $ "  Dia de semana mas activo: " ++ diaSemanaActivo eventos
    "b" -> do
      let antiguo  = eventoMasAntiguo eventos
          reciente = eventoMasReciente eventos
      putStrLn "\n  Evento mas antiguo:"
      mostrarEvento antiguo
      putStrLn "  Evento mas reciente:"
      mostrarEvento reciente
    "c" -> resumenPorIntervalo eventos
    _   -> putStrLn "\n  Opcion no valida."
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
    _   -> putStrLn "  No se exporto ningun reporte."
  eventosNuevos <- generarEventos eventos
  menuLoop eventosNuevos

manejarOpcion _ "0" = putStrLn "\nHasta luego!\n"

manejarOpcion eventos _ = do
  putStrLn "\nOpcion no valida. Intente de nuevo."
  menuLoop eventos

-- | Loop principal del sistema. Muestra el menú, lee la opción y la despacha.
--   La lista de eventos se pasa como parámetro en cada iteración
--   manteniendo el estado sin variables mutables.
--   Entrada:  lista de eventos actual
--   Salida:   IO () - recursa indefinidamente hasta que el usuario elige salir
menuLoop :: [Evento] -> IO ()
menuLoop eventos = do
  mostrarMenu
  opcion <- getLine
  manejarOpcion eventos opcion

-- | Punto de entrada del programa.
--   Configura el buffer de salida, genera los eventos iniciales y arranca el menú.
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Iniciando sistema..."
  eventosIniciales <- generarEventos []
  menuLoop eventosIniciales
