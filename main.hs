import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)

-- Objeto para representar la información de los Articulos
data Articulo = Articulo { nombre :: String, categoria :: String, stock :: Int }
    deriving (Show, Read)

-- Función cargar inventario
cargarInventario :: IO [Articulo]
cargarInventario = do
    contenido <- withFile "Inventario.txt" ReadMode $ \h -> do
        contenido <- hGetContents h
        contenido `deepseq` return contenido
    putStrLn "Contenido del archivo:"
    putStrLn contenido  -- Verifica el contenido leído
    let lineas = lines contenido
    return (map leerArticulo lineas)
    where
        leerArticulo :: String -> Articulo
        leerArticulo linea = case reads linea of
            [(articulo, "")] -> articulo
            _ -> error $ "Formato de artículo incorrecto: " ++ linea

-- Función Mostrar inventario
mostrarInventario :: Articulo -> String
mostrarInventario (Articulo nombre categoria stock) =
    "Articulo: nombre = \"" ++ nombre ++ "\", categoria = \"" ++ categoria ++ "\", stock = " ++ show stock ++ ""

-- Función para listar los artículos en el inventario
listarInventario :: [Articulo] -> IO ()
listarInventario [] = putStrLn "No hay artículos en el inventario."
listarInventario articulos = do
    putStrLn "Artículos en el inventario:"
    mapM_ (putStrLn . mostrarInventario) articulos

-- Función principal del ciclo
cicloPrincipal :: [Articulo] -> IO ()
cicloPrincipal inventario = do
    putStrLn "\nSeleccione una opción"
    putStrLn "1. Ingresar artículo"
    putStrLn "2. Buscar artículo por categoría"
    putStrLn "3. Listar todos los artículos"
    putStrLn "4. Mostrar cantidad de artículos por categorías"
    putStrLn "5. Salir"

    opcion <- getLine

    case opcion of
        "1" -> do
            putStrLn "\nIngresar artículo"
            putStrLn "Ingresar articulo nuevo"
            putStrLn "Ingresar artículo existente"

            opcionIngresar <- getLine

            case opcionIngresar of
                "1" -> do
                putStrLn "Ingresar nombre del articulo nuevo"
                nombre <- getLine
                putStrLn "Ingresar categoria del articulo nuevo"
                categoria <- getLine
                putStrLn "Ingresar stock del articulo nuevo"
                stock <- readLn
                 cicloPrincipal inventario
                

            cicloPrincipal inventario
        
        "2" -> do
            putStrLn "Buscar artículo por categoría"
            cicloPrincipal inventario
        
        "3" -> do
            putStrLn "Listar todos los artículos"
            listarInventario inventario
            cicloPrincipal inventario
        
        "4" -> do
            putStrLn "Mostrar cantidad de artículos por categorías"
            cicloPrincipal inventario
        
        "5" -> putStrLn "Saliendo del programa..."
        
        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal inventario

main :: IO ()
main = do
    inventario <- cargarInventario
    cicloPrincipal inventario