import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)
import Data.Maybe (fromJust, isJust)

-- Objeto para representar la información de los Articulos
data Articulo = Articulo { nombre :: String, categoria :: String, stock :: Int }
    deriving (Show, Read)

-- Función para guardar la información de los vehículos en un archivo de texto
guardarInventario :: [Articulo] -> IO ()
guardarInventario inventario = do
    withFile "inventario.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarInventario inventario))
    putStrLn "Inventario guardado en el archivo inventario.txt."

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
    "Articulo {nombre = \"" ++ nombre ++ "\", categoria = \"" ++ categoria ++ "\", stock = " ++ show stock ++ "}"

-- Función para listar los artículos en el inventario
listarInventario :: [Articulo] -> IO ()
listarInventario [] = putStrLn "No hay artículos en el inventario."
listarInventario articulos = do
    putStrLn "Artículos en el inventario:"
    mapM_ (putStrLn . mostrarInventario) articulos

-- Función para buscar un articulo
buscarArticulo :: String -> [Articulo] -> Maybe Articulo
buscarArticulo articuloInventario inventario =
    find (\v -> articuloInventario == nombre v) inventario


-- Función para buscar una categoría
eliminarArticulo :: String -> [Articulo] -> [Articulo]
eliminarArticulo nombreArticulo inventario =
    filter (\v -> nombre v /= nombreArticulo) inventario
    
-- Función para buscar una categoría
buscarCategoria :: String -> [Articulo] -> [Articulo]
buscarCategoria categoriaInventario inventario =
    filter (\v -> categoriaInventario == categoria v) inventario

 -- Función para buscar e imprimir un Articulo si lo encuentra
buscarYImprimirArticulo :: String -> [Articulo] -> Bool -> IO ()
buscarYImprimirArticulo categoriaInventario inventario estado = do
    let resultados = buscarCategoria categoriaInventario inventario
    let contador = length resultados
    if null resultados
        then putStrLn "No se encontraron artículos."
        else do
            putStrLn $ "Se encontraron: "++show contador ++" artículos en la categoria "++ categoriaInventario
            if estado
                then mapM_ print resultados
                else putStrLn ""

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
            putStrLn "\nIngresar artículo:"
            putStrLn "1. Ingresar articulo nuevo"
            putStrLn "2. Ingresar artículo existente"

            opcionIngresar <- getLine

            case opcionIngresar of
                "1" -> do
                    putStrLn "Ingresar nombre del articulo nuevo"
                    nombre <- getLine
                    let articulo = buscarArticulo nombre inventario
                    if isJust articulo
                        then do 
                            putStrLn "El articulo ya existe"
                            cicloPrincipal inventario
                        else do
                        putStrLn "Ingresar categoria del articulo nuevo"
                        categoria <- getLine
                        putStrLn "Ingresar stock del articulo nuevo"
                        stock <- readLn :: IO Int
                        let nuevoArticulo = Articulo nombre categoria stock
                        let inventarioActualizado = nuevoArticulo : inventario
                        guardarInventario inventarioActualizado
                        cicloPrincipal inventarioActualizado
                "2" -> do
                    putStrLn "Ingresar nombre del articulo existente"
                    nombre <- getLine
                    let articuloExistente = buscarArticulo nombre inventario
                    putStrLn "Ingresar cuánto de este artículo quieres agregar al sistema"
                    stockN <- readLn :: IO Int
                    if isJust articuloExistente
                        then do
                            let articulo = fromJust articuloExistente
                            let stockActual = stock articulo
                            let stockNuevo = stockActual + stockN
                            let articuloActualizado = articulo { stock = stockNuevo }
                            let inventarioSinArticulo = eliminarArticulo nombre inventario
                            let inventarioActualizado = articuloActualizado : inventarioSinArticulo
                            guardarInventario inventarioActualizado
                            cicloPrincipal inventarioActualizado
                        else do
                            putStrLn "El artículo no existe en el inventario."
                            cicloPrincipal inventario
                _ -> do
                    putStrLn "Opción no válida"
                    cicloPrincipal inventario
        
        "2" -> do
            putStrLn "Buscar artículo por categoría"
            putStrLn "Ingresar categoria del articulo a buscar"
            categoria <- getLine
            buscarYImprimirArticulo categoria inventario True
            cicloPrincipal inventario
        
        "3" -> do
            putStrLn "Listar todos los artículos"
            listarInventario inventario
            cicloPrincipal inventario
        
        "4" -> do
            putStrLn "Mostrar cantidad de artículos por categorías"
            putStrLn "Ingresar categoria del articulo a buscar"
            categoria <- getLine
            buscarYImprimirArticulo categoria inventario False
            cicloPrincipal inventario
        
        "5" -> putStrLn "Saliendo del programa..."
        
        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal inventario

main :: IO ()
main = do
    inventario <- cargarInventario
    cicloPrincipal inventario