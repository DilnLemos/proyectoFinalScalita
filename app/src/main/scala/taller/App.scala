/*
 * This Scala source file was generated by the Gradle 'init' task.
 */
package taller
import org.scalameter._
import scala.util.Random

object App {
  def main(args: Array[String]): Unit = {
    //println(greeting())

    val RiegoOptimo = new RiegoOptimo()

    val longitud_fincas = 10

    /*for (i <- 1 to longitud_fincas){
      val f = RiegoOptimo.fincaAlAzar(i);
      val d = RiegoOptimo.distanciaAlAzar(i)
      val timeSeq = withWarmer(new Warmer.Default) measure { RiegoOptimo.generarProgramacionesRiego(f) }
      val timePar = withWarmer(new Warmer.Default) measure { RiegoOptimo.generarProgramacionesRiegoPar(f) }
      val ratio = timeSeq.value / timePar.value
      println(s"$timeSeq & $timePar & $ratio & $i")
    } */
  //pruebas de tiempo de ejecución para CostoRiegoFinca
    /*for (i: Int <- 1 to longitud_fincas by 2) yield {

      val finca_creada = RiegoOptimo.fincaAlAzar(i)
      val prog_riego = RiegoOptimo.generarProgramacionesRiego(finca_creada)

      val programacion_aleatoria = prog_riego(Random.nextInt(prog_riego.length))
      println("Programación elegida para finca de tamaño " + (i) + " = " + programacion_aleatoria)

      val timeSeq = measure {
        RiegoOptimo.costoRiegoFinca(finca_creada, programacion_aleatoria)
      }
      val timePar = measure {
        RiegoOptimo.costoRiegoFincaPar(finca_creada, programacion_aleatoria)
      }
      println(s"Secuencial: $timeSeq ms")
      println(s"Paralelo: $timePar ms")
    }*/

  //pruebas de tiempo de ejecución para ProgramacionRiegoOptimo
    /*for(i: Int <- 1 to longitud_fincas by 2) yield {
      val finca = RiegoOptimo.fincaAlAzar(i)
      val distancia = RiegoOptimo.distanciaAlAzar(i)
      val timeSeq = measure {
        RiegoOptimo.ProgramacionRiegoOptimo(finca, distancia)
      }
      val timePar = measure {
        RiegoOptimo.ProgramacionRiegoOptimoPar(finca, distancia)
      }
      println(s"prueba de tieempo de ejecucion con $i tablones:")
      println(s"Tiempo secuencial: $timeSeq ms")
      println(s"Tiempo paralelo: $timePar ms")}
  */
  }

  def greeting(): String = "Hello, world!"
}