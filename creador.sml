fun crearArchivo () =
    let
        val nombreArchivo = "matricula.csv"
    in
        (let
            val archivo = TextIO.openAppend nombreArchivo
            val _ = TextIO.closeOut archivo
        in
            print "Archivo ya existe\n"
        end)

    handle _ => (* Si ocurre un error por ejemplo,  si el archivo no existe,
                entramos en este bloque "handle" lo usamos para crear el archivo
                y escribir el encabezado inicial. *)

        let
            val archivo = TextIO.openOut nombreArchivo
            val _ = TextIO.output(archivo, "carnet_estudiante,nombre,curso,creditos,costo_credito\n")
            val _ = TextIO.closeOut archivo
        in
            print "Archivo creado con encabezado\n"
        end
end;

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

fun leerDatos () =
    let
        val _ = print "Carnet: "
        val carnet = limpiar (valOf (TextIO.inputLine TextIO.stdIn))

        val _ = print "Nombre: "
        val nombre = limpiar (valOf (TextIO.inputLine TextIO.stdIn))

        val _ = print "Curso: "
        val curso = limpiar (valOf (TextIO.inputLine TextIO.stdIn))

        val _ = print "Creditos: "
        val creditos = limpiar (valOf (TextIO.inputLine TextIO.stdIn))
        val creditos = valOf (Int.fromString creditos)

        val _ = print "Costo: "
        val costo = limpiar (valOf (TextIO.inputLine TextIO.stdIn))
        val costo = valOf (Real.fromString costo)
    in
        (carnet, nombre, curso, creditos, costo)
    end;


fun agregarEstudiante (carnet, nombre, curso, creditos, costo) =
    let
        val linea = carnet ^ "," ^ nombre ^ "," ^ curso ^ "," ^ Int.toString creditos ^ "," ^ Real.toString costo ^ "\n"
        val archivo = TextIO.openAppend "matricula.csv"
        val _ = TextIO.output(archivo, linea)
        val _ = TextIO.closeOut archivo
    in
        ()
    end;