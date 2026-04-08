use "creador.sml";

fun main () =
    let
        val _ = crearArchivo ()
        val datos = leerDatos ()
        val _ = agregarEstudiante datos
    in
        print "Todo listo\n"
    end;