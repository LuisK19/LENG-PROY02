module Main (main) where

import System.IO     (hSetBuffering, stdout, BufferMode(..))
import System.Random (randomRIO)
import Data.Time     (getCurrentTime, utctDay, toGregorian, Day)

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
-- Convierte un índice 0-4 a Categoria 
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

-- Muestra un evento
mostrarEvento :: Evento -> IO ()
mostrarEvento e = putStrLn $
  "  ID: "    ++ show (eventoId e)  ++
  " | Cat: "  ++ show (categoria e) ++
  " | Valor: " ++ show (valor e)    ++
  " | TS: "   ++ show (timestamp e)


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
  putStrLn "\n[Transformacion - por implementar]"
  eventosNuevos <- generarEventos eventos
  menuLoop eventosNuevos

manejarOpcion eventos "2" = do
  putStrLn "\n[Analisis de datos - por implementar]"
  eventosNuevos <- generarEventos eventos
  menuLoop eventosNuevos

manejarOpcion eventos "3" = do
  putStrLn "\n[Analisis temporal - por implementar]"
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