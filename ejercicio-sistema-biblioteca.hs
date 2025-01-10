-- Definición de las estructuras
data Libro = Libro {
    titulo :: String,
    autor :: String,
    isbn :: String
} deriving (Show)

data Usuario = Usuario {
    nombre :: String,
    idUsuario :: Int,
    librosPrestados :: [Libro]
} deriving (Show)

data Biblioteca = Biblioteca {
    nombreBiblioteca :: String,
    catalogo :: [Libro],
    usuarios :: [Usuario]
} deriving (Show)

crearLibro :: String -> String -> String -> Libro
crearLibro titulo autor isbn = Libro titulo autor isbn

crearUsuario :: String -> Int -> [Libro] -> Usuario
crearUsuario nombre idUsuario libros = Usuario nombre idUsuario libros

-- Función para añadir varios libros al catálogo de la biblioteca
añadirLibros :: [Libro] -> Biblioteca -> Biblioteca
añadirLibros libros biblioteca = biblioteca { catalogo = libros ++ catalogo biblioteca }

-- Función para añadir un usuario a la biblioteca
añadirUsuarios :: [Usuario] -> Biblioteca -> Biblioteca
añadirUsuarios usuariosl biblioteca = biblioteca { usuarios = usuariosl ++ usuarios biblioteca }

main :: IO ()
main = do
    let libro1 = crearLibro "El Quijote" "Miguel de Cervantes" "123456789"
    let libro2 = crearLibro "La Divina Comedia" "J. Bernardino de Literatura" "987654321"
    let biblioteca = Biblioteca "Biblioteca Unam" [] []
    let libros = [libro1, libro2]
    let usuario1 = crearUsuario "Juan Perez" 1 libros
    let usuario2 = crearUsuario "Juan Sanitorio" 2 libros
    let usuarios = [usuario1, usuario2]
    let bibliotecaConLibro = añadirLibros libros biblioteca
    let bibliotecaConUsuario = añadirUsuarios usuarios bibliotecaConLibro
    print bibliotecaConUsuario
