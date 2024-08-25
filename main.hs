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
    withFile "inventario.txt" WriteMode $ \h -> do --Selecciona el archivo a leer
        hPutStr h (unlines (map mostrarInventario inventario)) --Utiliza la función mostrarInventario para dar el formato en el que se guarda la información
    putStrLn "Inventario guardado en el archivo inventario.txt."

-- Función cargar inventario
cargarInventario :: IO [Articulo]
cargarInventario = do
    contenido <- withFile "Inventario.txt" ReadMode $ \h -> do --Seleciona el archivo a modificar
        contenido <- hGetContents h
        contenido `deepseq` return contenido
    let lineas = lines contenido
    return (map leerArticulo lineas)
    where
        --Función que crea los artículos según un formato y devuelve error si el formato está incorrecto
        leerArticulo :: String -> Articulo
        leerArticulo linea = case reads linea of
            [(articulo, "")] -> articulo
            _ -> error $ "Formato de artículo incorrecto: " ++ linea

-- Función Mostrar inventario donde se define el formato con el que se muetra y guardan los datos
mostrarInventario :: Articulo -> String
mostrarInventario (Articulo nombre categoria stock) =
    "Articulo {nombre = \"" ++ nombre ++ "\", categoria = \"" ++ categoria ++ "\", stock = " ++ show stock ++ "}"

-- Función para listar los artículos en el inventario
listarInventario :: [Articulo] -> IO ()
listarInventario [] = putStrLn "No hay artículos en el inventario."
listarInventario articulos = do
    putStrLn "Artículos en el inventario:"
    mapM_ (putStrLn . mostrarInventario) articulos --Aplica las funciones mostrarInventario (que devuelve un String) y putStrLn a cada articulo de la lista

-- Función para buscar un articulo
buscarArticulo :: String -> [Articulo] -> Maybe Articulo --Devuelve un Articulo de encontrar uno que cumpla la condición
buscarArticulo articuloBuscar inventario =
    find (\v -> articuloBuscar == nombre v) inventario --Busca que articuloBuscar sea igual al nombre del Articulo

-- Función para buscar una categoría
eliminarArticulo :: String -> [Articulo] -> [Articulo] 
eliminarArticulo nombreArticulo inventario =
    filter (\v -> nombre v /= nombreArticulo) inventario --Filtra el contenido y crea una nueva lista con los datos que son distintos al producto buscado
    
-- Función para buscar una categoría
buscarCategoria :: String -> [Articulo] -> [Articulo]
buscarCategoria categoriaInventario inventario =
    filter (\v -> categoriaInventario == categoria v) inventario --Busca todos los articulos que son de la misma categoria

 -- Función para buscar e imprimir un Articulo si lo encuentra
buscarEImprimirArticulo :: String -> [Articulo] -> Bool -> IO ()
buscarEImprimirArticulo categoriaInventario inventario estado = do
    let resultados = buscarCategoria categoriaInventario inventario --Llama la función buscarCategoria y almacena el resultado en resultados
    let contador = length resultados --Tamaño de array resultados
    if null resultados
        then putStrLn "No se encontraron artículos."
        else do
            putStrLn $ "Se encontraron: "++show contador ++" artículos en la categoria "++ categoriaInventario
            if estado --Al ser True imprime la listra resultados
                then mapM_ print resultados
                else putStrLn ""

-- Función principal del ciclo (cada que se "termina" una de las opciones se vuelve a llamar a ella misma y se termina)
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
                    let articulo = buscarArticulo nombre inventario --Llama la función buscarArticulo y le pasa el nombre ingresado y  el inventario actual
                    if isJust articulo --Confirma si existe, si es el caso, devuelve que el artículo ya existe y no modifica el inventario
                        then do 
                            putStrLn "El articulo ya existe"
                            cicloPrincipal inventario
                        else do --Si el artículo no existe pide los datos restantes, crea el artículo y lo guarda en el archivo txt
                        putStrLn "Ingresar categoria del articulo nuevo"
                        categoria <- getLine
                        putStrLn "Ingresar stock del articulo nuevo"
                        stock <- readLn :: IO Int
                        let nuevoArticulo = Articulo nombre categoria stock
                        let inventarioActualizado = nuevoArticulo : inventario
                        guardarInventario inventarioActualizado
                        cicloPrincipal inventarioActualizado --Vuelve a iniciar pasandole el inventario actualizado
                "2" -> do
                    putStrLn "Ingresar nombre del articulo existente"
                    nombre <- getLine
                    let articuloExistente = buscarArticulo nombre inventario --Busca que el artículo si exista
                    if isJust articuloExistente --Si existe entonces pide el stock y lo modifica
                        then do
                            putStrLn "Ingresar cuánto de este artículo quieres agregar al sistema"
                            stockN <- readLn :: IO Int --Recoge el stock deseado
                            let articulo = fromJust articuloExistente --Crea una copia articulo
                            let stockActual = stock articulo --Recoge el stock actual
                            let stockNuevo = stockActual + stockN --Calcula el nuevo stock
                            let articuloActualizado = articulo { stock = stockNuevo } --Crea un nuevo artículo con el stock actualizado
                            let inventarioSinArticulo = eliminarArticulo nombre inventario --Elimina el articulo viejo
                            let inventarioActualizado = articuloActualizado : inventarioSinArticulo --Agrega el articulo nuevo y tin, se manda
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
            buscarEImprimirArticulo categoria inventario True --Se muestra los articulos encontrados
            cicloPrincipal inventario
        
        "3" -> do
            putStrLn "Listar todos los artículos"
            listarInventario inventario
            cicloPrincipal inventario
        
        "4" -> do
            putStrLn "Mostrar cantidad de artículos por categorías"
            putStrLn "Ingresar categoria del articulo a buscar"
            categoria <- getLine
            buscarYImprimirArticulo categoria inventario False --No se muestra los articulos encontrados
            cicloPrincipal inventario
        
        "5" -> putStrLn "Saliendo del programa..."
        
        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal inventario

main :: IO ()
main = do
    inventario <- cargarInventario
    cicloPrincipal inventario