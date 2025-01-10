import System.IO ()

-- Definición de las estructuras
data Estudiante = Estudiante
    { nombre :: String
    , edad   :: Int
    } deriving (Show)

type RegistroEstudiantes = [Estudiante]

agregarEstudiante :: Estudiante -> RegistroEstudiantes -> RegistroEstudiantes
agregarEstudiante estudiante registro = estudiante : registro

buscarEstudiante :: String -> RegistroEstudiantes -> Maybe Estudiante
buscarEstudiante _ [] = Nothing
buscarEstudiante nombre (e@(Estudiante n _):resto)
    | nombre == n = Just e
    | otherwise = buscarEstudiante nombre resto

mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn "Seleccione una opción:"
    putStrLn "1. Agregar estudiante"
    putStrLn "2. Buscar estudiante"
    putStrLn "3. Salir"
    putStrLn ""

main :: IO ()
main = do
    let registro :: RegistroEstudiantes = []
    loop registro

loop :: RegistroEstudiantes -> IO ()
loop registro = do
    mostrarMenu
    putStr "Ingresar opcion: "
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el nombre del estudiante:"
            nombre <- getLine
            putStrLn "Ingrese la edad del estudiante:"
            edadStr <- getLine
            let edad = read edadStr :: Int
            let nuevoEstudiante = Estudiante nombre edad
            let registroActualizado = agregarEstudiante nuevoEstudiante registro
            print registroActualizado
            putStrLn ""
            loop registroActualizado

        "2" -> do
            putStrLn "Ingrese el nombre del estudiante a buscar:"
            nombreBuscar <- getLine
            putStrLn ""
            putStrLn "Resultado de la busqueda:"
            let resultado = buscarEstudiante nombreBuscar registro
            print resultado
            putStrLn ""
            loop registro

        "3" -> do
            putStrLn "Saliendo del programa..."
            return ()

        _ -> do
            putStrLn "Opción inválida, intente de nuevo."
            putStrLn ""
            loop registro
