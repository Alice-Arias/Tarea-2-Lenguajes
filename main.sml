use "creador.sml";
use "analizador.sml";

fun leerEntrada mensaje =
    let
        val _ = print mensaje
        val linea = valOf (TextIO.inputLine TextIO.stdIn)
    in
        String.substring(linea, 0, size linea - 1)
    end;

(* 
Objetivo: Verificar si el archivo existe, si no crearlo con encabezado.
Entrada: No recibe parámetros.
Salida: Archivo creado o mensaje indicando que ya existe.
Validaciones: Maneja error si el archivo no existe usando handle.
*)
fun crearArchivo () =
    let
        val nombreArchivo = "matricula.csv"
    in
        ( let
            val archivo = TextIO.openAppend nombreArchivo
            val _ = TextIO.closeOut archivo
        in
            (print "\n------------------\n";
            print "Archivo ya existe\n";
            print "------------------\n")

        end )

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

fun menuPrincipal () =
    let
        val _ = print "\n====================================\n"
        val _ = print "        SISTEMA DE MATRICULA        \n"
        val _ = print "====================================\n"
        val _ = print "  1. Creador\n"
        val _ = print "  2. Analizador\n"
        val _ = print "  3. Salir\n"
        val _ = print "------------------------------------\n"
        val opcion = leerEntrada "Seleccione una opcion: "
    in
        case opcion of
            "1" =>
                (menu [];menuPrincipal ())

            | "2" =>
                (analizarArchivo (); menuPrincipal ())

            | "3" =>
                print "\n Saliendo del sistema\n\n"

            | _ => (print "\n!! Opcion invalida, intente de nuevo\n";menuPrincipal ())
    end;


fun main () =
    let
        val _ = crearArchivo ()
    in
        menuPrincipal ()
    end;

val _ = main ();