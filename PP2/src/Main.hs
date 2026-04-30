module Main (main) where

import System.IO (hSetBuffering, stdout, BufferMode(..))

-- Tipo suma
data Categoria
  = Visualizacion
  | Apartado
  | Compra
  | Devolucion
  | Seguimiento
  deriving (Show, Eq, Ord)

-- Tipo producto
data Evento = Evento
  { eventoId  :: Int       
  , categoria :: Categoria 
  , valor     :: Double    
  , timestamp :: Int      
  } deriving (Show, Eq)


-- ==========================  MENÚ PRINCIPAL  ==================================
-- Muestra las opciones en pantalla
mostrarMenu :: IO ()
mostrarMenu = do
  putStrLn "\n=============================="
  putStrLn "  Sistema de Eventos PP2"
  putStrLn "=============================="
  putStrLn "1. Transformación de eventos"
  putStrLn "2. Análisis de datos"
  putStrLn "3. Análisis temporal"
  putStrLn "4. Búsqueda"
  putStrLn "5. Estadísticas"
  putStrLn "0. Salir"
  putStrLn "==============================\n"
  putStr "Seleccione una opción: "

manejarOpcion :: [Evento] -> String -> IO ()
manejarOpcion eventos "1" = do
  putStrLn "\n[Transformación - por implementar]"
  menuLoop eventos
manejarOpcion eventos "2" = do
  putStrLn "\n[Análisis de datos - por implementar]"
  menuLoop eventos
manejarOpcion eventos "3" = do
  putStrLn "\n[Análisis temporal - por implementar]"
  menuLoop eventos
manejarOpcion eventos "4" = do
  putStrLn "\n[Búsqueda - por implementar]"
  menuLoop eventos
manejarOpcion eventos "5" = do
  putStrLn "\n[Estadísticas - por implementar]"
  menuLoop eventos
manejarOpcion _ "0" = putStrLn "\nHasta luego!\n"
manejarOpcion eventos _ = do
  putStrLn "\nOpción no válida. Intente de nuevo."
  menuLoop eventos

-- Loop principal: muestra menú, lee opción, repite
menuLoop :: [Evento] -> IO ()
menuLoop eventos = do
  mostrarMenu
  opcion <- getLine
  manejarOpcion eventos opcion

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering  
  let eventosIniciales = []         -- lista vacía por ahora
  menuLoop eventosIniciales