import java.time.Period
import scala.language.noAutoTupling
class Pelicula(
  val titulo: String,
  val duracionMinutos: Int,
  val clasificacion: String
){
} 
object Pelicula {
    def apply(titulo: String, duracionMinutos: Int, clasificacion: String): Pelicula = {
        if duracionMinutos <= 0 then
            new Pelicula(titulo, 90, clasificacion ) 
        else
            new Pelicula(titulo, duracionMinutos, clasificacion)
    }
}

def mayorI (a: Int, b: Int): Int ={
    if a > b then a else b
}
def mayorD (a: Double, b: Double): Double ={
    if a > b then a else b
}

class FuncionCine(
  val pelicula: Pelicula,
  val sala: Int,
  val hora: String,
  val precioEntrada: Double,
  val asientosDisponibles: Int
){
    def puedeReservar(cantidad: Int): Boolean = {
        cantidad <= asientosDisponibles
    }

    def costoReserva(cantidad: Int): Double = {
        cantidad.toDouble*precioEntrada
    }

    //To do, Que hago si me mandan un negativo?
    def reservar(cantidad: Int): FuncionCine = {
        if cantidad <= asientosDisponibles then
            FuncionCine(pelicula, sala, hora, precioEntrada, asientosDisponibles - cantidad)
        else 
            this
    }

    def descripcion: String = {
        s"${pelicula.titulo}\nSala: $sala\nHora: $hora\nPrecio: $precioEntrada Bs\nAsientos disponibles: $asientosDisponibles"
    }
}
object FuncionCine{
    def apply(pelicula: Pelicula, sala: Int, hora: String, precioEntrada: Double, asientosDisponibles: Int): FuncionCine = {
        new FuncionCine(pelicula, mayorI(sala,1), hora, mayorD(precioEntrada,0.0), mayorI(asientosDisponibles,0))
    }

    //To do, Asi?
    def funcionEstandar( pelicula: Pelicula, sala: Int, hora: String): FuncionCine = {
        FuncionCine(pelicula, sala, hora, 30, 50)
    }
}

object parte2 {
    def main (args: Array[String]): Unit = {
        val pelicula1 = Pelicula("Dune",155,"PG-13")
        val funcion1 = FuncionCine(pelicula1,3,"20:30",35.0,40)
        val funcion2 = FuncionCine.funcionEstandar(pelicula1, 1, "16:00")

        println(funcion1.descripcion)
        println(funcion1.puedeReservar(5))
        println(funcion1.costoReserva(5))
        val funcionReservada = funcion1.reservar(5)
        println(funcion1.asientosDisponibles)
        println(funcionReservada.asientosDisponibles)
    }
}

