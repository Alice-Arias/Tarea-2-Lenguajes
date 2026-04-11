

(* 
Objetivo: Limpiar un texto eliminando el salto de línea final.
Entrada: texto (string)
Salida: texto sin salto de línea al final.
Validaciones: Verifica que el texto no esté vacío antes de procesar.
*)
fun limpiar texto =
    let
        val longitud = String.size texto
        val ultimoCaracter = String.sub (texto, longitud - 1)
    in
        (* Se verifica primero que el texto no esté vacío y además que el último carácter sea un salto de línea.
        Se usa "andalso" para evaluar ambas condiciones *)
        if longitud > 0 andalso ultimoCaracter = #"\n"
        then String.substring (texto, 0, longitud - 1)
        else texto
    end;

(* 
Objetivo: Leer el nombre del usuario desde consola.
Entrada: No recibe parámetros.
Salida: Nombre válido (string).
Validaciones: No permite que el nombre esté vacío.
*)
fun leerNombre () =
    let
        val _ = print "\n------------------------------------\n"
        val _ = print "Nombre: "
        val entrada = limpiar (valOf (TextIO.inputLine TextIO.stdIn))
    in
        if String.size entrada > 0 
        then entrada
        else ( print "\nError: El nombre no puede estar vacio\n"; leerNombre ())
    end;

(* 
Objetivo: Verificar si una cadena contiene solo números.
Entrada: cadena (string)
Salida: true si todos son dígitos, false si no.
Validaciones: Revisa cada carácter de la cadena.
Se usa para validar el carnet que debe contener solo números y tener una longitud específica.
*)
fun esNumero cadena =
    List.all Char.isDigit (String.explode cadena);

(* 
Objetivo: Leer el carnet del estudiante.
Entrada: No recibe parámetros.
Salida: Carnet válido (string).
Validaciones: Debe tener exactamente 10 dígitos numéricos.
*)
fun leerCarnet () =
    let
        val _ = print "\n------------------------------------\n"
        val _ = print "Carnet: "
        val entrada = limpiar (valOf (TextIO.inputLine TextIO.stdIn))
    in
        if String.size entrada = 10 andalso esNumero entrada
        then entrada
        else (print "\nError: El carnet debe tener 10 digitos numericos\n";leerCarnet ())
    end;

(* 
Objetivo: Leer el código del curso.
Entrada: No recibe parámetros.
Salida: Código de curso válido (string).
Validaciones: Debe tener exactamente 6 caracteres.
*)
fun leerCurso () =
    let
        val _ = print "\n------------------------------------\n"
        val _ = print "Curso: "
        val entrada = limpiar (valOf (TextIO.inputLine TextIO.stdIn))
    in
        if String.size entrada = 6 
        then entrada
        else (print "\nError: El curso debe tener exactamente 6 caracteres\n";leerCurso ())
    end;

(* 
Objetivo: Leer la cantidad de créditos.
Entrada: No recibe parámetros.
Salida: Número entero de créditos (int).
Validaciones: Debe ser un número válido.
*)
fun leerCreditos () =
    let
        val _ = print "\n------------------------------------\n"
        val _ = print "Creditos: "
        val entrada = limpiar (valOf (TextIO.inputLine TextIO.stdIn))
    in
        case Int.fromString entrada 
        of
            SOME numero => numero (*Si la conversión es exitosa, se devuelve el número convertido**)
        |   NONE =>(print "\nError: Numero invalido\n";leerCreditos ()) (**Si la conversión falla, se muestra un mensaje de error y se vuelve a llamar a la función para intentar nuevamente.**)
    end;

(* 
Objetivo: Leer el costo por crédito.
Entrada: No recibe parámetros.
Salida: Número real (real).
Validaciones: Debe ser un valor numérico válido.
*)
fun leerCosto () =
    let
        val _ = print "\n------------------------------------\n"
        val _ = print "Costo: "
        val entrada = limpiar (valOf (TextIO.inputLine TextIO.stdIn))
        val _ = print "\n------------------------------------\n"
    in
        case Real.fromString entrada 
        of
            SOME numero => numero
        |   NONE => (print "\nError: Valor invalido\n";leerCosto ())
    end;

(* 
Objetivo: Leer todos los datos de un estudiante.
Entrada: No recibe parámetros.
Salida: Tupla con los datos (string * string * string * int * real).
Validaciones: Usa funciones individuales que ya validan cada campo.
*)
fun leerDatos () =
    let
        val carnetEstudiante = leerCarnet ()
        val nombreEstudiante = leerNombre ()
        val codigoCurso = leerCurso ()
        val cantidadCreditos = leerCreditos ()
        val costoPorCredito = leerCosto ()
    in
        (carnetEstudiante, nombreEstudiante, codigoCurso, cantidadCreditos, costoPorCredito)
    end;

(* 
Objetivo: Guardar un estudiante en el archivo CSV.
Entrada: Tupla (carnet, nombre, curso, creditos, costo).
Salida: No retorna valor (unit).
Validaciones: Convierte correctamente los datos a formato string.
*)
fun agregarEstudiante (carnet, nombre, curso, creditos, costo) =
    let
        val lineaCSV =carnet ^ "," ^ nombre ^ "," ^ curso ^ "," ^Int.toString creditos ^ "," ^Real.fmt (StringCvt.FIX (SOME 2)) costo ^ "\n"

        val archivo = TextIO.openAppend "matricula.csv"
        val _ = TextIO.output(archivo, lineaCSV)
        val _ = TextIO.closeOut archivo
    in
        ()
    end;

(* 
Objetivo: Limpiar el catálogo (archivo CSV).
Entrada: No recibe parámetros.
Salida: Archivo reiniciado con encabezado.
Validaciones: Sobrescribe completamente el archivo.
*)
fun limpiarCatalogo () =
    let
        val nombreArchivo = "matricula.csv"
        val archivo = TextIO.openOut nombreArchivo

        val _ = TextIO.output(archivo, "carnet_estudiante,nombre,curso,creditos,costo_credito\n")
        val _ = TextIO.closeOut archivo
    in
        (print "-----------------------------------\n";
        print "Se limpio el catalogo correctamente\n";
        print "-----------------------------------\n")
    end;

(* 
Objetivo: Mostrar menú interactivo del sistema.
Entrada: lista de estudiantes en memoria.
Salida: Lista actualizada de estudiantes.
Validaciones: Controla opciones inválidas y mantiene ejecución continua.
*)
fun menu lista =
    let
        val _ = print "\n====================================\n"
        val _ = print "     SISTEMA DE MATRICULA CREADOR\n"
        val _ = print "====================================\n"
        val _ = print "1. Agregar estudiante\n"
        val _ = print "2. Limpiar catalogo\n"
        val _ = print "3. Volver\n"
        val _ = print "------------------------------\n"
        val _ = print "Seleccione una opcion: "

        val opcion = limpiar (valOf (TextIO.inputLine TextIO.stdIn))
    in
        case opcion of
            "1" =>
                let
                    val _ = print "\n------------------------------------\n"
                    val _ = print "       Matricular Estudiante\n"
                    val _ = print "------------------------------------\n"
                    val datos = leerDatos ()
                    val _ = agregarEstudiante datos
                    val nuevaLista = datos :: lista
                    val _ = print "\nEstudiante agregado correctamente\n"
                in
                    menu nuevaLista
                end

        | "2" =>
                let
                    val _ = limpiarCatalogo ()
                in
                    menu []   (* reinicia lista también ya que la guardo en memoria por si quiero hacer mas matriculas sin que se cierre el programa*)
                end

        | "3" =>
                (print "\n Volviendo al menu principal\n";lista)

        | _ =>
                (print "\n Opcion invalida\n";menu lista)
    end;