fun leerEntrada mensaje =
    let
        val _ = print mensaje
        val linea = valOf (TextIO.inputLine TextIO.stdIn)
    in
        String.substring(linea, 0, size linea - 1)
    end;

fun obtenerRutaValida () =
    let
        val _ = print "\n==========================================\n"
        val ruta = leerEntrada "Ingrese la ruta del archivo: "
    in
        (
            let
                val archivo = TextIO.openIn ruta
                val _ = TextIO.closeIn archivo
            in
                ruta
            end
        )
        handle _ =>
            (print "\n!! Error: archivo no existe o no se puede abrir\n\n";obtenerRutaValida ())
    end;

fun analizarArchivo () =
    let
        val ruta = obtenerRutaValida ()
    in
        menuAnalizador ruta
    end

(* Menú del analizador con ruta guardada *)
and menuAnalizador ruta =
    let
        val _ = print "\n============ ANALIZADOR ============\n"
        val _ = print ("Archivo: " ^ ruta ^ "\n")
        val _ = print "------------------------------------\n"
        val _ = print "1. Cursos con mayor ingreso\n"
        val _ = print "2. Cursos con mas de 5 estudiantes\n"
        val _ = print "3. Buscar por estudiante\n"
        val _ = print "4. Cursos por creditos\n"
        val _ = print "5. Resumen general\n"
        val _ = print "6. Volver\n"
        val _ = print "------------------------------------\n"

        val opcion = leerEntrada "Seleccione: "
    in
        case opcion of
            "1" => (print "\n[Mayor ingreso]\n"; menuAnalizador ruta)
        | "2" => (print "\n[+5 estudiantes]\n"; menuAnalizador ruta)
        | "3" => (print "\n[Busqueda]\n"; menuAnalizador ruta)
        | "4" => (print "\n[Por creditos]\n"; menuAnalizador ruta)
        | "5" => (print "\n[Resumen]\n"; menuAnalizador ruta)
        | "6" => print "\nVolviendo...\n"
        | _   => (print "\nOpcion invalida\n"; menuAnalizador ruta)
    end;