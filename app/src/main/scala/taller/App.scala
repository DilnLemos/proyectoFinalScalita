/*
 * This Scala source file was generated by the Gradle 'init' task.
 */
package taller
import org.scalameter._
import scala.util.Random

object App {
  def main(args: Array[String]): Unit = {
    println(greeting())

    val RiegoOptimo = new RiegoOptimo()

    val longitud_fincas = 10

    println("\\begin{center}\n\\begin{tabular}{ |c|c|c| } \n\\hline")
    for (i <- 1 to longitud_fincas){
      val f = RiegoOptimo.fincaAlAzar(i);
      val d = RiegoOptimo.distanciaAlAzar(i)
      val timeSeq = withWarmer(new Warmer.Default) measure { RiegoOptimo.generarProgramacionesRiego(f) }
      val timePar = withWarmer(new Warmer.Default) measure { RiegoOptimo.generarProgramacionesRiegoPar(f) }
      val ratio = timeSeq.value / timePar.value
      println(s"$timeSeq & $timePar & $ratio & $i")
    } 
    println("\\hline\n\\end{tabular}\n\\end{center}")
  }

  def greeting(): String = "Hello, world!"
}
