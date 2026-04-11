(* 
Objetivo: Leer una entrada del usuario desde consola.
Entrada: mensaje a mostrar.
Salida: string sin salto de línea.
Validaciones: elimina el \n al final para evitar errores en comparaciones.
*)
(*
Objetivo: Leer una entrada del usuario desde consola.
Entrada: mensaje a mostrar.
Salida: string válido sin salto de línea.
Validaciones: no permite entradas vacías.
*)
fun leerEntrada mensaje =
    let
        val _ = print mensaje
        val linea = valOf (TextIO.inputLine TextIO.stdIn)
        val limpio = String.substring(linea, 0, size linea - 1)
    in
        if limpio = "" orelse limpio = " "
        then
            (print "\nError: no puedes dejarlo vacio.\n";leerEntrada mensaje)
        else
            limpio
    end;


(* 
Objetivo: Solicitar una ruta de archivo válida.
Entrada: ninguna.
Salida: ruta válida del archivo.
Validaciones: si el archivo no existe o no se puede abrir, vuelve a pedir la ruta.
*)
fun obtenerRutaValida () =
    let
        val _ = print "\n==========================================\n"
        val ruta = leerEntrada "Ingrese la ruta del archivo: "
    in
        (let
                val archivo = TextIO.openIn ruta
                val _ = TextIO.closeIn archivo
            in
                ruta
            end)

        handle _ => (* Si ocurre un error por ejemplo,  si el archivo no existe,
                entramos en este bloque "handle" lo usamos para crear el archivo
                y escribir el encabezado inicial. *)

            (print "\n!! Error: archivo no existe o no se puede abrir\n\n"; obtenerRutaValida ())
    end;

(*========================================================================================================================*)

(* 
Objetivo: Limpiar caracteres no deseados de un string.
Entrada: string.
Salida: string limpio (sin saltos de línea).
*)
fun limpiarCampo s =
    String.implode (
        List.filter (fn c => c <> #"\n" andalso c <> #"\r") (String.explode s)
    );

(* 
Objetivo: Dividir una línea CSV en partes usando comas.
Entrada: línea completa.
Salida: lista de strings.
*)
fun dividirLinea linea =
    String.tokens (fn c => c = #",") linea;

(* 
Objetivo: Convertir una línea CSV en una tupla estructurada.
Entrada: línea de texto.
Salida: SOME tupla si es válida, NONE si hay error.
Validaciones: verifica cantidad de campos y convierte tipos.
*)
fun pasarLinea linea =
    case dividirLinea linea 
    of
        [carnet, nombre, curso, creditos, costo] =>
            SOME (
                limpiarCampo carnet,
                limpiarCampo nombre,
                limpiarCampo curso,
                valOf (Int.fromString (limpiarCampo creditos)),
                valOf (Real.fromString (limpiarCampo costo))
            )
    | _ =>
        (print ("\nLinea invalida: " ^ linea ^ "\n"); NONE);

(* 
Objetivo: Leer todas las líneas de un archivo.
Entrada: archivo abierto.
Salida: lista de líneas.
*)
fun leerLineas archivo =
    case TextIO.inputLine archivo 
    of
        NONE => []
        | SOME linea => linea :: leerLineas archivo;

(* 
Objetivo: Leer archivo CSV y convertirlo a lista de datos.
Entrada: ruta del archivo.
Salida: lista de tuplas (matrículas).
Validaciones: ignora encabezado y líneas inválidas.
*)
fun leerCSV ruta =
    let
        val archivo = TextIO.openIn ruta
        val lineas = leerLineas archivo
        val _ = TextIO.closeIn archivo
    in
        List.mapPartial pasarLinea (List.tl lineas) (* Ignora la primera línea (encabezado) *)
    end;

(* 
Objetivo: Calcular ingreso generado por una matrícula.
Entrada: tupla de matrícula.
Salida: monto total (real).
*)
fun ingreso (_, _, _, creditos, costo) =
    Real.fromInt creditos * costo;

(* 
Objetivo: Acumular ingresos por curso.
Entrada: curso, monto, lista acumulada.
Salida: lista actualizada.
Validaciones: si el curso ya existe, suma; si no, lo agrega.
*)
fun acumular (cursoNuevo : string, montoNuevo : real, listaCursos : (string * real) list) =
    case listaCursos of
        [] => [(cursoNuevo, montoNuevo)]

    | (cursoExistente, montoExistente)::resto =>
            if cursoNuevo = cursoExistente 
            then (cursoExistente, montoExistente + montoNuevo) :: resto

            else
                (cursoExistente, montoExistente) :: acumular (cursoNuevo, montoNuevo, resto);

(* 
Objetivo: Calcular ingresos totales por cada curso.
Entrada: lista de matrículas.
Salida: lista (curso, ingreso total).
*)
fun ingresosPorCurso listaMatriculas =
    List.foldl (**Recorre la lista y acumula los ingresos por curso**)
        (fn (matriculaActual, acumulado) =>
            let
                val (_, _, cursoActual, _, _) = matriculaActual
                val montoActual = ingreso matriculaActual
            in
                acumular (cursoActual, montoActual, acumulado)
            end )
        [] listaMatriculas;

(* 
Objetivo: Ordenar cursos por ingreso (descendente).
Entrada: lista de cursos.
Salida: lista ordenada.
Método: quicksort.
*)
fun ordenar (listaCursos : (string * real) list) =
    case listaCursos of
        [] => []

    | (cursoBase, montoBase) :: restoCursos =>
            let
                val cursosMayores = List.filter (fn (_, monto) => monto > montoBase) restoCursos

                val cursosMenores = List.filter (fn (_, monto) => monto <= montoBase) restoCursos
            in
                ordenar cursosMayores @ [(cursoBase, montoBase)] @ ordenar cursosMenores
            end;

(*
Objetivo: Leer un número real válido desde consola
Entrada: mensaje
Salida: real válido
Validaciones: evita errores de conversión
*)
fun leerReal mensaje =
    let
        val entrada = leerEntrada mensaje
    in
        case Real.fromString entrada of
            SOME valor => valor
        | NONE =>
                (print "\nError: ingrese un numero valido.\n";leerReal mensaje)
    end;

(* 
Objetivo: Mostrar ranking de cursos por ingreso dentro de un rango.
Entrada: ruta del archivo.
Salida: impresión en pantalla.
Validaciones: filtra por monto mínimo y máximo.
*)
fun cursosMayorIngreso rutaArchivo =
    let
        val listaMatriculas = leerCSV rutaArchivo
        val _ = print "\n==========================================\n"
        val _ = print "Ranking de cursos por ingreso\n"
        val _ = print "=========================================="

        val montoMinimo = leerReal "\nMonto minimo: "
        val montoMaximo = leerReal "\nMonto maximo: "

        val listaCursos = ingresosPorCurso listaMatriculas

        val cursosFiltrados =
            List.filter
                (fn (_, montoTotal) => montoTotal >= montoMinimo andalso montoTotal <= montoMaximo) listaCursos

        val cursosOrdenados = ordenar cursosFiltrados

        val _ = print "\n========= Ranking de Cursos =========\n\n"

        fun imprimirCursos [] = ()
            | imprimirCursos ((nombreCurso, montoTotal) :: restoCursos) =
                (print ("Curso: " ^ nombreCurso ^ " | Ingreso: CRC " ^ Real.fmt (StringCvt.FIX (SOME 2)) montoTotal ^ "\n");imprimirCursos restoCursos)

        val _ =
            if null cursosOrdenados 
            then print "\nNo hay cursos en ese rango\n"
            else
                ()
    in
        imprimirCursos cursosOrdenados
    end;

(*========================================================================================================================*)

(* 
Objetivo: Agregar un estudiante a un curso sin repetirlo.
Entrada: curso, carnet, lista acumulada.
Salida: lista actualizada.
*)
fun agregarEstudianteCurso (cursoNuevo, carnetNuevo, listaCursos) =
    case listaCursos of
        [] => [(cursoNuevo, [carnetNuevo])]

    | (cursoExistente, listaCarnets) :: resto =>
            if cursoNuevo = cursoExistente 
            then
                if List.exists (fn c => c = carnetNuevo) listaCarnets 
                then (cursoExistente, listaCarnets) :: resto

                else
                    (cursoExistente, carnetNuevo :: listaCarnets) :: resto
            else
                (cursoExistente, listaCarnets) :: agregarEstudianteCurso (cursoNuevo, carnetNuevo, resto);

(* 
Objetivo: Agrupar estudiantes por curso (sin repetir).
Entrada: lista de matrículas.
Salida: lista (curso, lista de carnets).
*)
fun estudiantesPorCurso listaMatriculas =
    List.foldl
        (fn ((carnet, _, curso, _, _), acumulado) => agregarEstudianteCurso (curso, carnet, acumulado))
        [] listaMatriculas;

(* 
Objetivo: Mostrar cursos con 5 o más estudiantes diferentes.
Entrada: ruta del archivo.
Salida: impresión en pantalla.
*)
fun cursosConMasDe5Estudiantes rutaArchivo =
    let
        val listaMatriculas = leerCSV rutaArchivo

        val cursosConEstudiantes = estudiantesPorCurso listaMatriculas

        val cursosFiltrados = List.filter (fn (_, listaCarnets) => length listaCarnets >= 5) cursosConEstudiantes

        val _ = print "\n===== Cursos con mas de 5 estudiantes =====\n\n"

        fun imprimir [] = ()
            | imprimir ((curso, listaCarnets) :: resto) =
                    (print ("Curso: " ^ curso ^
                    " | Estudiantes: " ^
                    Int.toString (length listaCarnets) ^ "\n");
                    imprimir resto)
        val _ =
            if null cursosFiltrados 
            then print "\nNo hay cursos con mas de 5 estudiantes\n"
            else
                ()
    in
        imprimir cursosFiltrados
    end;

(*========================================================================================================================*)

(*
Objetivo: Buscar matrículas por carnet exacto o nombre parcial.
Entrada: lista de matrículas y texto de búsqueda.
Salida: lista filtrada de matrículas.
Validaciones: compara carnet exacto o si el nombre contiene el texto.
*)
fun buscarPorEstudiante listaMatriculas busqueda =
    List.filter
        (fn (carnet, nombre, curso, creditos, costo) =>
            carnet = busqueda orelse String.isSubstring busqueda nombre )
        listaMatriculas;

(*
Objetivo: Imprimir resultados de búsqueda de matrículas.
Entrada: lista de matrículas filtradas.
Salida: impresión en consola.
Validaciones: si la lista está vacía, informa al usuario.
*)
fun imprimirBusqueda [] = ()

    | imprimirBusqueda ((carnet, nombre, curso, creditos, costo) :: resto) =
        (
            print (
                "Carnet: " ^ carnet ^
                " | Nombre: " ^ nombre ^
                " | Curso: " ^ curso ^
                " | Creditos: " ^ Int.toString creditos ^
                " | Costo: CRC " ^
                Real.fmt (StringCvt.FIX (SOME 2)) costo ^ "\n"
            );
            imprimirBusqueda resto
        );

(*
Objetivo: Mostrar los resultados de búsqueda de matrículas.
Entrada: lista de matrículas filtradas.
Salida: impresión en consola de los resultados o mensaje si no hay coincidencias.
Validaciones: verifica si la lista está vacía antes de imprimir.
*)
fun mostrarResultadosBusqueda resultados =
    if null resultados 
    then
        print "\nNo se encontraron resultados\n"

    else (print "\n===== Resultados de Busqueda =====\n\n";
        imprimirBusqueda resultados)

(*
Objetivo: Buscar matrículas asociadas a un estudiante (por carnet exacto o nombre parcial).
Entrada: ruta del archivo CSV.
Salida: imprime en pantalla todas las matrículas que coinciden con la búsqueda.
Validaciones: la búsqueda puede ser por coincidencia exacta (carnet) o parcial (nombre).
*)
fun buscarMatriculasEstudiante rutaArchivo =
    let
        val _ = print "\n==========================================\n"

        val listaMatriculas = leerCSV rutaArchivo

        val _ = print "=================================================================\n"
        val _ = print "Buscar por carnet (exacto) o nombre del estudiante (parcial)\n"
        val _ = print "=================================================================\n\n"
        val busqueda = leerEntrada "Ingrese carnet o nombre del estudiante: "

        val resultados = buscarPorEstudiante listaMatriculas busqueda
    in
        mostrarResultadosBusqueda resultados
    end;

(*========================================================================================================================*)

(*
Objetivo: Buscar cursos que tengan una cantidad específica de créditos.
Entrada: lista de matrículas y número de créditos a buscar.
Salida: lista de cursos que coinciden con ese número de créditos.
Validaciones: filtra solo por igualdad de créditos.
*)
fun buscarCursosPorCreditos listaMatriculas creditos =
    List.map (fn (_, _, curso, _, _) => curso)
        (List.filter
            (fn (_, _, _, c, _) => c = creditos)
            listaMatriculas);

(*
Objetivo: Imprimir una lista de cursos en pantalla.
Entrada: lista de cursos (strings).
Salida: salida en consola.
Validaciones: si la lista está vacía, se detiene recursivamente.
*)
fun imprimirCursos [] = ()

    | imprimirCursos (curso :: resto) =
        (print ("Curso: " ^ curso ^ "\n");
            imprimirCursos resto);

(*
Objetivo: Mostrar los resultados de búsqueda de cursos por créditos.
Entrada: lista de cursos.
Salida: impresión en consola.
Validaciones: verifica si la lista está vacía antes de imprimir.
*)
fun mostrarResultadosBusquedaCursos resultados =
    if null resultados 
    then
        print "\nNo se encontraron resultados\n"
        
    else (print "\n===== Resultados de Busqueda =====\n\n";
        imprimirCursos resultados
    );

(*
Objetivo: Ejecutar la opción de búsqueda de cursos por créditos.
Entrada: ruta del archivo CSV.
Salida: imprime en consola los cursos que coinciden con los créditos ingresados.
Validaciones: convierte la entrada a entero y reutiliza funciones de búsqueda.
*)
fun opcionCursosPorCreditos ruta =
    let
        val listaMatriculas = leerCSV ruta

        val _ = print "\n==========================================\n"
        val _ = print "Buscar cursos por la cantidad de creditos\n"
        val _ = print "==========================================\n\n"

        val busqueda = leerEntrada "Ingrese la cantidad de creditos: "
        val creditos = valOf (Int.fromString busqueda)

        val resultado = buscarCursosPorCreditos listaMatriculas creditos
    in
        mostrarResultadosBusquedaCursos resultado
    end;

(*========================================================================================================================*)

(* 
Objetivo: Iniciar el módulo analizador.
Entrada: ninguna.
Salida: muestra menú analizador.
*)
fun analizarArchivo () =
    let
        val ruta = obtenerRutaValida ()
    in
        menuAnalizador ruta
    end

(*========================================================================================================================*)

(* 
Objetivo: Mostrar menú del analizador.
Entrada: ruta del archivo.
Salida: ejecución de opciones seleccionadas.
Validaciones: controla opciones inválidas.
*)
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
            "1" => (cursosMayorIngreso ruta; menuAnalizador ruta)
        | "2" => (cursosConMasDe5Estudiantes ruta; menuAnalizador ruta)
        | "3" => (buscarMatriculasEstudiante ruta; menuAnalizador ruta)
        | "4" => (opcionCursosPorCreditos ruta; menuAnalizador ruta)
        | "5" => (print "\n[Pendiente]\n"; menuAnalizador ruta)
        | "6" => print "\nVolviendo...\n"
        | _   => (print "\nOpcion invalida\n"; menuAnalizador ruta)
    end;