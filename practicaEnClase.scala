import javax.management.Notification
//Ejercicio 1
trait Figura {
    def area: Double
}

class Rectangulo (
    val altura: Double,
    val base: Double
) extends Figura {
    def area: Double =  base*altura
}

class Circulo (
    val radio: Double
) extends Figura {
    def area: Double = 3.1416*(radio*radio)
}

class Triangulo (
    val base: Double,
    val altura: Double
) extends Figura{
 def area: Double = (base*altura)/2
}

def mostrarArea (figura: Figura): Double = {
    figura.area
}

//Ejercicio 2
abstract class Animal(
  val nombre: String
) {
  def sonido(): String
}

trait Volador {
    def volar: String
}

trait Nadador {
    def nadar: String
}

class Perro (
    nombre: String
)extends Animal(nombre){
    def sonido(): String = "Gau!"
}

class Pato (
    nombre: String
) extends Animal(nombre) with Nadador with Volador{
    def sonido(): String = "Cuac!"
    def nadar: String = "Nado pe causa"
    def volar: String = "Masomenos vuelo pe causa"
}

class Pez (
    nombre: String
) extends Animal(nombre) with Nadador {
    def sonido(): String = "Glu glu!"
    def nadar: String = "Nado pe glu"
}

//Ejericio 3
abstract class Empleado(
  val nombre: String,
  val salarioBase: Double
) {
  def salarioTotal: Double
}

class Desarollador (
    nombre: String,
    salarioBase: Double,
    val bono: Double
) extends Empleado(nombre, salarioBase) {
    def salarioTotal: Double = salarioBase + bono
}

class Vendedor(
    nombre: String,
    salarioBase: Double,
    val comision: Double,
    val cantidadDeVentas: Int
) extends Empleado(nombre, salarioBase) {
    def salarioTotal: Double = salarioBase + (comision*cantidadDeVentas.toDouble)
}

class Gerente(
    nombre: String,
    salarioBase: Double,
    val porcentaje: Double
) extends Empleado(nombre, salarioBase) {
    def salarioTotal: Double = salarioBase + (salarioBase*porcentaje)
}

//Ejercio 4
trait Notificacion {
  def enviar(mensaje: String): String
}

class Email () extends Notificacion {
    def enviar(mensaje: String): String = "Enviando Correo: " + mensaje
}

class SMS () extends Notificacion {
    def enviar(mensaje: String): String = "Enviando SMS: "+ mensaje
}

class Push () extends Notificacion {
    def enviar(mensaje: String): String = "Enviando Notificación: "+ mensaje
}

def procesarNotificacion(notificacion: Notificacion, mensaje: String): String = {
    notificacion.enviar(mensaje)
}

//Ejericio 5
abstract class Vehiculo(
  val marca: String,
  val velocidadMaxima: Double
){
    def descripcion(): String
}

trait recargable{
    def recargar: String
}

trait electrico {
    def electrico: String
}

class Automovil(
    marca: String,
    velocidadMaxima: Double
)extends Vehiculo(marca, velocidadMaxima) with recargable{
    def recargar: String = "Soy recargable con Gasolina"
    def descripcion(): String = s"Marca: $marca\nVelocidad max: $velocidadMaxima\n$recargar"
}

class Motocicleta(
    marca: String,
    velocidadMaxima: Double
)extends Vehiculo(marca, velocidadMaxima) with recargable{
    def recargar: String = "Soy recargable con Gasolina"
    def descripcion(): String = s"Marca: $marca\nVelocidad max: $velocidadMaxima\n$recargar"
}

class AutoElectrico(
    marca: String,
    velocidadMaxima: Double
)extends Vehiculo(marca, velocidadMaxima) with recargable with electrico{
    def recargar: String = "Soy recargable con electricidad"
    def electrico: String = "No necesito gasolina"
    def descripcion(): String = s"Marca: $marca\nVelocidad max: $velocidadMaxima\n$recargar\n$electrico"
}

//Reto
abstract class Cuenta(
  val titular: String,
  val saldo: Double
) {
  def depositar(monto: Double): Cuenta
  def retirar(monto: Double): Cuenta
}

class CuentaDeAhorro(
    titular: String,
    saldo: Double,
    val retiroDisponibles: Int = 4
) extends Cuenta (titular, saldo) {
    def aplicarInteres(interes: Double): Cuenta = {
        new CuentaDeAhorro(titular, saldo +(saldo*interes),retiroDisponibles)
    } 
    def depositar(monto: Double): Cuenta = {
        if monto < 0 then
            this
        else 
            new CuentaDeAhorro(titular, saldo + monto, retiroDisponibles)
    }
    def retirar(monto: Double): Cuenta = {
        if retiroDisponibles < 1 then
            this
        if monto < 0 then
            this
        else if monto > saldo then
            this 
        else    
            new CuentaDeAhorro(titular, saldo - monto, retiroDisponibles-1)
    }
}

class CuentaCorriente(
    titular: String,
    saldo: Double,
) extends Cuenta (titular, saldo) {
    def depositar(monto: Double): Cuenta = {
        if monto < 0 then
            this
        else 
            new CuentaDeAhorro(titular, saldo + monto)
    }
    def retirar(monto: Double): Cuenta = {
        if monto < 0 then
            this
        else if monto > saldo then
            this 
        else    
            new CuentaDeAhorro(titular, saldo - monto)
    }
}

object practicaEnClase {
    def main (args: Array[String]): Unit = {
        println("Ejercicio 1")
        val rec = new Rectangulo(4,4)
        val cir = new Circulo(4)
        val tri = new Triangulo(4,4)
        println(mostrarArea(rec))
        println(mostrarArea(cir))
        println(mostrarArea(tri))
        println()

        println("Ejercicio 2")
        val pepe = new Perro("Pepe")
        val donalado = new Pato("Donaldo")
        val nemo = new Pez("Nemo")
        println(nemo.nadar)
        println(donalado.volar)
        println(pepe.sonido())
        println()

        println("Ejercicio 3")
        val des = new Desarollador("Juan", 4300, 700)
        val ven = new Vendedor("Kadir", 3300, 150, 3)
        val ger = new Gerente("Miranda", 5300, 0.2)
        println(des.salarioTotal)
        println(ven.salarioTotal)
        println(ger.salarioTotal)
        println()

        println("Ejercicio 4")
        val eml = new Email
        val sms = new SMS
        val psh = new Push
        println(procesarNotificacion(sms,"Cou págame de una vez"))
        println()

        println("Ejercicio 5")
        val aut = new Automovil("Kia", 150.0)
        val mot = new Motocicleta("Yamaha", 200.0)
        val ate = new AutoElectrico("Tesla", 250.0)
        println(aut.descripcion())
        println(mot.descripcion())
        println(ate.descripcion())
        println()

        
        println("Reto")
        val per = new CuentaDeAhorro("Cou", 3400)
        val per1 = per.depositar(3400)
        println(per1.saldo)
        }
}

