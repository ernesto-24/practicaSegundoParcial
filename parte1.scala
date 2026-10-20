object part1 {

    //Ejercicio 1
    val duplicar: Int => Int = x => x * 2
    val cuadrado: Int => Int = x => x * x 
    val esPar:Int => Boolean = x => x%2 == 0

    //Ejercicio 2
    val cubo:                     Int => Int = x => x * x * x
    val promedio: (Double, Double) => Double = (a,b) => (a+b)/2
    val mayor:             (Int, Int) => Int = (a,b) => if a > b then a else b
    val esMultiploDeTres:     Int => Boolean = x => (x%3) == 0

    //Ejercico 3
    def transformar(numero: Int, operacion: Int => Int): Int = {
        operacion(numero)
    }

    //Ejercicio 4
    def calcular(a: Double, b: Double, operacion: (Double, Double) => Double): Double = {
        operacion(a,b)
    }
    val sumar:       (Double, Double) => Double = (a,b) => a + b
    val restar:      (Double, Double) => Double = (a,b) => a - b
    val multiplicar: (Double, Double) => Double = (a,b) => a * b
    val dividir:     (Double, Double) => Double = (a,b) => a / b

    //Ejercicio 5
    def crearMultiplicador(factor: Int): Int => Int = {
        (x: Int) => x * factor
    }
    val duplicar_ej5: Int => Int = crearMultiplicador(2)
    val triplilcar: Int => Int = crearMultiplicador(3)
    val porDiez: Int => Int = crearMultiplicador(10)

    //Ejericio 6
    def aplicarDescuento(precio: Double, porcentaje: Double): Double = {
        precio - (precio * porcentaje)
    }
    val descuento10: Double => Double  = aplicarDescuento(_: Double,0.1)
    val descuento20: Double => Double  = aplicarDescuento(_: Double,0.2)
    val descuento30: Double => Double  = aplicarDescuento(_: Double,0.3)

    //Ejercicio 7
    def elevar(exponente: Int)(base: Int): Int = {
        if exponente == 0 then 1 else
            elevar(exponente-1)(base) * base
    }
    val cuadrado_ej7    = elevar(2)
    val cubo_ej7        = elevar(3)
    val cuartaPotencia  = elevar(4)

    //Ejercicio 8
    val limpiar: String => String        = cad => cad.trim()
    val convertirMinusculas: String => String = cad => cad.toLowerCase()
    val longitud: String => Int          = cad => cad.length()
    val esLongitudValida: Int => Boolean = num => num >= 5
    val validarTexto: String => Boolean  =  esLongitudValida.compose(longitud.compose(convertirMinusculas.compose(limpiar)))

    //Ejercicio 9
    def sumar_ej9 (desde: Int, hasta: Int, transformar: Int => Int): Int = {
        if desde > hasta then 0 else 
            transformar(desde) + sumar_ej9(desde+1, hasta, transformar)
    }
    def producto (desde: Int, hasta: Int, transformar: Int => Int): Int = {
        if desde > hasta then 1 else 
            transformar(desde) * producto(desde+1, hasta, transformar)
    }
    val opNormal:   Int => Int = x => x
    val opCuadrada: Int => Int = x => x * x
    val opCubica:   Int => Int = x => x * x * x

    //Ejercicio 10
    def transformarTexto(texto: String,transformacion: Char => Char): String = {
        if texto.isEmpty() then "" else 
            transformacion(texto.head) + transformarTexto(texto.tail, transformacion)
    }
    def buscarCaracter(texto: String,condicion: Char => Boolean): Boolean = {
        if 
            texto.isEmpty() then false 
        else if 
            condicion(texto.head) then true 
        else 
            buscarCaracter(texto.tail, condicion)
    }
    val paMinus:      Char => Char = c => c.toLower
    val tieneDigi: Char => Boolean = c => c.isDigit
    




    def main(args: Array[String]): Unit = {

        println("Ejercicio 1")
        println(duplicar(19))
        println(cuadrado(12))
        println(esPar(67))
        println()

        println("Ejercicio 2")
        println(cubo(3))
        println(promedio(65.8,68.2))
        println(mayor(1,-999999))
        println(esMultiploDeTres(67))
        println()

        println("Ejercicio 3")
        println(transformar(5,duplicar))
        println(transformar(5,cuadrado))
        println(transformar(5,x => x + 10))
        println(transformar(5,x => x - 2*x))
        println()

        println("Ejercicio 4")
        println(calcular(6,7,sumar))
        println(calcular(6,7,restar))
        println(calcular(6,7,multiplicar))
        println(calcular(6,7,dividir))
        println(calcular(6,7,promedio))
        println()

        println("Ejercicio 5")
        println(duplicar_ej5(7))
        println(triplilcar(7))
        println(porDiez(7))
        println()     

        println("Ejercicio 6")
        println(descuento10(500))
        println(descuento20(500))
        println(descuento30(500))
        println()

        println("Ejericio 7")
        println(cuadrado_ej7(5))
        println(cubo_ej7(3))
        println(cuartaPotencia(2))
        println()

        println("Ejercicio 8")
        println(validarTexto(" Como es "))
        println()

        println("Ejercicio 9 Primera Parte")
        println(sumar_ej9(1,4,opNormal))
        println(sumar_ej9(1,4,opCuadrada))
        println(sumar_ej9(1,4,opCubica))
        println()

        println("Ejercicio 9 Segunda Parte")
        println(producto(1,4,opNormal))
        println(producto(1,4,opCuadrada))
        println(producto(1,4,opCubica))
        println()

        println("Ejercicio 10")
        println(transformarTexto("SACALA",paMinus))
        println(buscarCaracter("SACALA",tieneDigi))

    }



}