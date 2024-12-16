package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

@RunWith(classOf[JUnitRunner])
class RiegoOptimoTest extends AnyFunSuite {
  val objRiego = new RiegoOptimo()

  test("Test Calculo de Costo Riego de finca con 5 Tablones #1") {
    val finca = Vector((10, 3, 4), (5, 4, 4), (2, 2, 1), (8, 8, 1), (6, 4, 2))
    val programacion_riego = Vector(2, 4, 0, 3, 1)
    val calculo = objRiego.costoRiegoFinca(finca, programacion_riego)
    assert(calculo == 74)
  }

  test("Test Calculo de Costo Riego de finca con 3 Tablones #2") {
    val finca = Vector((1, 3, 1), (4, 2, 2), (3, 3, 2))
    val programacion_riego = Vector(2, 1, 0)
    val calculo = objRiego.costoRiegoFinca(finca, programacion_riego)
    assert(calculo == 9)
  }

  test("Test Calculo de Costo Riego de finca con 8 Tablones #3") {
    val finca = Vector((13, 8, 1), (1, 2, 1), (11, 5, 4), (8, 7, 3), (3, 4, 1), (5, 6, 2), (3, 7, 2), (10, 4, 4))
    val programacion_riego = Vector(1, 2, 7, 6, 4, 5, 0, 3)
    val calculo = objRiego.costoRiegoFinca(finca, programacion_riego)
    assert(calculo == 232)
  }

  test("Test Calculo de Costo Riego de finca con 9 Tablones #4") {
    val finca = Vector((18, 6, 2), (6, 8, 4), (14, 4, 3), (7, 6, 3), (7, 8, 3), (12, 2, 2), (9, 5, 2), (16, 8, 2), (11, 5, 4))
    val programacion_riego = Vector(2, 0, 7, 5, 6, 8, 3, 4, 1)
    val calculo = objRiego.costoRiegoFinca(finca, programacion_riego)
    assert(calculo == 528)
  }

  test("Test Calculo de Costo Riego de finca con 10 Tablones #5") {
    val finca = Vector((9, 8, 2), (18, 1, 1), (6, 4, 3), (11, 9, 3), (4, 3, 3), (3, 7, 2), (20, 9, 1), (4, 8, 3), (10, 9, 4), (19, 8, 2))
    val programacion_riego = Vector(0, 6, 9, 1, 5, 7, 2, 8, 3, 4)
    val calculo = objRiego.costoRiegoFinca(finca, programacion_riego)
    assert(calculo == 830)
  }

  test("Test Calculo de Costo Riego (paralelizado) de finca con 5 Tablones #1") {
    val finca = Vector((10, 3, 4), (5, 4, 4), (2, 2, 1), (8, 8, 1), (6, 4, 2))
    val programacion_riego = Vector(2, 4, 0, 3, 1)
    val calculo = objRiego.costoRiegoFincaPar(finca, programacion_riego)
    assert(calculo == 74)
  }

  test("Test Calculo de Costo Riego (paralelizado) de finca con 3 Tablones #2") {
    val finca = Vector((1, 3, 1), (4, 2, 2), (3, 3, 2))
    val programacion_riego = Vector(2, 1, 0)
    val calculo = objRiego.costoRiegoFincaPar(finca, programacion_riego)
    assert(calculo == 9)
  }

  test("Test Calculo de Costo Riego (paralelizado) de finca con 8 Tablones #3") {
    val finca = Vector((13, 8, 1), (1, 2, 1), (11, 5, 4), (8, 7, 3), (3, 4, 1), (5, 6, 2), (3, 7, 2), (10, 4, 4))
    val programacion_riego = Vector(1, 2, 7, 6, 4, 5, 0, 3)
    val calculo = objRiego.costoRiegoFincaPar(finca, programacion_riego)
    assert(calculo == 232)
  }

  test("Test Calculo de Costo Riego (paralelizado) de finca con 9 Tablones #4") {
    val finca = Vector((18, 6, 2), (6, 8, 4), (14, 4, 3), (7, 6, 3), (7, 8, 3), (12, 2, 2), (9, 5, 2), (16, 8, 2), (11, 5, 4))
    val programacion_riego = Vector(2, 0, 7, 5, 6, 8, 3, 4, 1)
    val calculo = objRiego.costoRiegoFincaPar(finca, programacion_riego)
    assert(calculo == 528)
  }

  test("Test Calculo de Costo Riego (paralelizado) de finca con 10 Tablones #5") {
    val finca = Vector((9, 8, 2), (18, 1, 1), (6, 4, 3), (11, 9, 3), (4, 3, 3), (3, 7, 2), (20, 9, 1), (4, 8, 3), (10, 9, 4), (19, 8, 2))
    val programacion_riego = Vector(0, 6, 9, 1, 5, 7, 2, 8, 3, 4)
    val calculo = objRiego.costoRiegoFincaPar(finca, programacion_riego)
    assert(calculo == 830)
  }

  test("Generar Programaciones de Riego 5") {
    val f = Vector((10,3,4), (5,4,4), (2,2,1), (8,8,1), (6,4,2))
    val prog = objRiego.generarProgramacionesRiego(f)
    assert(prog == (0 until f.length).toVector.permutations.toVector, "si")
  }

  test("Generar Programaciones de Riego 1") {
    val f = Vector((10,3,4), (5,4,4), (8,8,1), (6,4,2))
    val prog = objRiego.generarProgramacionesRiego(f)
    assert(prog == (0 until f.length).toVector.permutations.toVector, "si")
  }

  test("Generar Programaciones de Riego 2") {
    val f = Vector((7,3,4), (6,4,4), (3,2,1), (8,2,1), (6,4,2), (8,2,4))
    val prog = objRiego.generarProgramacionesRiego(f)
    assert(prog == (0 until f.length).toVector.permutations.toVector, "si")
  }

  test("Generar Programaciones de Riego 3") {
    val f = Vector((10,3,4), (8,8,1), (6,4,2))
    val prog = objRiego.generarProgramacionesRiego(f)
    assert(prog == (0 until f.length).toVector.permutations.toVector, "si")
  }

  test("Generar Programaciones de Riego 4") {
    val f = Vector((10,3,4), (1,1,2), (3,8,9), (2,7,1), (2,8,3), (1,2,3), (3,2,1))
    val prog = objRiego.generarProgramacionesRiego(f)
    assert(prog == (0 until f.length).toVector.permutations.toVector, "si")
  }


  test("Generar Programaciones de Riego Par 1") {
    val f = Vector((10,3,4), (5,4,4), (2,2,1), (8,8,1), (6,4,2))
    val prog = objRiego.generarProgramacionesRiego(f)
    assert(prog == (0 until f.length).toVector.permutations.toVector.par.toVector, "si")
  }

  test("Generar Programaciones de Riego Par 2") {
    val f = Vector((10,3,4), (5,4,4), (8,8,1), (6,4,2))
    val prog = objRiego.generarProgramacionesRiego(f)
    assert(prog == (0 until f.length).toVector.permutations.toVector.par.toVector, "si")
  }

  test("Generar Programaciones de Riego Par 3") {
    val f = Vector((7,3,4), (6,4,4), (3,2,1), (8,2,1), (6,4,2), (8,2,4))
    val prog = objRiego.generarProgramacionesRiego(f)
    assert(prog == (0 until f.length).toVector.permutations.toVector.par.toVector, "si")
  }

  test("Generar Programaciones de Riego Par 4") {
    val f = Vector((10,3,4), (8,8,1), (6,4,2))
    val prog = objRiego.generarProgramacionesRiego(f)
    assert(prog == (0 until f.length).toVector.permutations.toVector.par.toVector, "si")
  }

  test("Generar Programaciones de Riego Par 5") {
    val f = Vector((10,3,4), (1,1,2), (3,8,9), (2,7,1), (2,8,3), (1,2,3), (3,2,1))
    val prog = objRiego.generarProgramacionesRiego(f)
    assert(prog == (0 until f.length).toVector.permutations.toVector.par.toVector, "si")
  }
  
  //tests para ProgramacionRiegoOptima
  test("test_1 ProgramaciónRiegoOptimo") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3))
    val distancias = Vector(Vector(0, 1, 2), Vector(1, 0, 1), Vector(2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptimo(finca, distancias)
    assert(res == (Vector(0, 1, 2), 13))
  }

  test("test_2 ProgramaciónRiegoOptimo") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3), (4, 4, 4))
    val distancias = Vector(Vector(0, 1, 2, 3), Vector(1, 0, 1, 2), Vector(2, 1, 0, 1), Vector(3, 2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptimo(finca, distancias)
    assert(res == (Vector(0, 1, 2, 3), 38))
  }

  test("test_3 ProgramaciónRiegoOptimo") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3), (4, 4, 4), (5, 5, 5))
    val distancias = Vector(Vector(0, 1, 2, 3, 4), Vector(1, 0, 1, 2, 3), Vector(2, 1, 0, 1, 2), Vector(3, 2, 1, 0, 1), Vector(4, 3, 2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptimo(finca, distancias)
    assert(res == (Vector(0, 1, 2, 3, 4), 89)) 
  }

  test("test_4 ProgramaciónRiegoOptimo") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3), (4, 4, 4), (5, 5, 5), (6, 6, 6))
    val distancias = Vector(Vector(0, 1, 2, 3, 4, 5), Vector(1, 0, 1, 2, 3, 4), Vector(2, 1, 0, 1, 2, 3), Vector(3, 2, 1, 0, 1, 2), Vector(4, 3, 2, 1, 0, 1), Vector(5, 4, 3, 2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptimo(finca, distancias)
    assert(res == (Vector(0, 1, 2, 3, 4, 5), 180))
  }

  test("test_5 ProgramaciónRiegoOptimo") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3), (4, 4, 4), (5, 5, 5), (6, 6, 6), (7, 7, 7))
    val distancias = Vector(Vector(0, 1, 2, 3, 4, 5, 6), Vector(1, 0, 1, 2, 3, 4, 5), Vector(2, 1, 0, 1, 2, 3, 4), Vector(3, 2, 1, 0, 1, 2, 3), Vector(4, 3, 2, 1, 0, 1, 2), Vector(5, 4, 3, 2, 1, 0, 1), Vector(6, 5, 4, 3, 2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptimo(finca, distancias)
    assert(res == (Vector(0, 1, 2, 3, 4, 5, 6), 328))
  }

//test para ProgramacionRiegoOptimo Paralelizada
  test("test_1 ProgramacionRiegoOptimo Paralelizada") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3))
    val distancias = Vector(Vector(0, 1, 2), Vector(1, 0, 1), Vector(2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptimoPar(finca, distancias)
    assert(res == (Vector(0, 1, 2), 13))
  }

  test("test_2 ProgramacionRiegoOptimo Paralelizada") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3), (4, 4, 4))
    val distancias = Vector(Vector(0, 1, 2, 3), Vector(1, 0, 1, 2), Vector(2, 1, 0, 1), Vector(3, 2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptimoPar(finca, distancias)
    assert(res == (Vector(0, 1, 2, 3),38))
  }

  test("test_3 ProgramacionRiegoOptimo Paralelizada") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3), (4, 4, 4), (5, 5, 5))
    val distancias = Vector(Vector(0, 1, 2, 3, 4), Vector(1, 0, 1, 2, 3), Vector(2, 1, 0, 1, 2), Vector(3, 2, 1, 0, 1), Vector(4, 3, 2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptimoPar(finca, distancias)
    assert(res == (Vector(0, 1, 2, 3, 4), 89))
  }

  test("test_4 ProgramacionRiegoOptimo Paralelizada") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3), (4, 4, 4), (5, 5, 5), (6, 6, 6))
    val distancias = Vector(Vector(0, 1, 2, 3, 4, 5), Vector(1, 0, 1, 2, 3, 4), Vector(2, 1, 0, 1, 2, 3), Vector(3, 2, 1, 0, 1, 2), Vector(4, 3, 2, 1, 0, 1), Vector(5, 4, 3, 2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptimoPar(finca, distancias)
    assert(res == (Vector(0, 1, 2, 3, 4, 5), 180))
  }

  test("test_5 ProgramacionRiegoOptimo Paralelizada") {
    val finca = Vector((1, 1, 1), (2, 2, 2), (3, 3, 3), (4, 4, 4), (5, 5, 5), (6, 6, 6), (7, 7, 7))
    val distancias = Vector(Vector(0, 1, 2, 3, 4, 5, 6), Vector(1, 0, 1, 2, 3, 4, 5), Vector(2, 1, 0, 1, 2, 3, 4), Vector(3, 2, 1, 0, 1, 2, 3), Vector(4, 3, 2, 1, 0, 1, 2), Vector(5, 4, 3, 2, 1, 0, 1), Vector(6, 5, 4, 3, 2, 1, 0))
    val res = objRiego.ProgramacionRiegoOptimoPar(finca, distancias)
    assert(res == (Vector(0, 1, 2, 3, 4, 5, 6), 328))
  }
}
