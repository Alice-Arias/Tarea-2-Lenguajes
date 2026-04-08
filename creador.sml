fun crearArchivo () =
    let
        val encabezadoCSV = "carnet_estudiante,nombre,curso,creditos,costo_credito\n"

        val archivoSalida = TextIO.openOut "matricula.csv"
        val _ = TextIO.output(archivoSalida, encabezadoCSV)
        val _ = TextIO.closeOut archivoSalida
    in
        print "El archivo matricula.csv ha sido creado con el encabezado\n"
    end;
