/**
 * Plantilla para pruebas
* @author Carlos Delgado
* @version 1.0
 */

package proyecto
import datos._

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestPruebasSeq5 extends AnyFunSuite{

  val itoObj = new Itinerario()
  val itsCurso = itoObj.itinerariosSalida(vuelosCurso , aeropuertosCurso)
  val itsCurso15 = itoObj.itinerariosSalida(vuelosA1, aeropuertos)
  val itsCurso40 = itoObj.itinerariosSalida(vuelosB1, aeropuertos)

  test("test1") {
    val its1 = itsCurso("CTG", "PTY", 11, 40)
    assert(its1 === List(Vuelo("COPA", 1234, "CTG", 10, 0, "PTY", 11, 30, 0)))
  }

  test ("test2") {
    val its2 = itsCurso("CTG", "PTY", 11, 55)
    assert(its2 === List(Vuelo("AVA", 4321, "CTG", 9, 30, "SMR", 10, 0, 0), Vuelo("COPA", 7631, "SMR", 10, 50, "PTY", 11, 50, 0)))
  }

  test ("test3") {
    val its3 = itsCurso("CTG", "PTY", 12, 30)
    assert(its3 === List(Vuelo("AVA", 4321, "CTG", 9, 30, "SMR", 10, 0, 0), Vuelo("COPA", 7631, "SMR", 10, 50, "PTY", 11, 50, 0)))
  }

  test("test4") {
    val its4 = itsCurso15("HOU", "BNA", 19,20)
    assert(its4 ===List(Vuelo("4X", 371, "HOU", 8, 35, "MSY", 10, 30, 1), Vuelo("AA", 828, "MSY", 17, 10, "BNA", 18, 37, 0)) )
  }

  test("test5") {
    val its5 = itsCurso40("DFW", "ATL",16, 5)
    assert(its5 === List(Vuelo("AA", 864, "DFW", 6, 56, "ATL", 15, 3, 0)))
  }

  test("test6") {
    val its5 = itsCurso40("DEN", "MIA",18, 53)
    assert(its5 === List(Vuelo("AA", 50, "DEN", 13, 20, "DFW", 16, 24, 0), Vuelo("AA", 498, "DFW", 14, 25, "MIA", 18, 4, 0)))
  }

  test("test7") {
    val its5 = itsCurso40("DFW", "HOU",16, 5)
    assert(its5 === List(Vuelo("AA", 926, "DFW", 23, 16, "HOU", 12, 15, 0)))
  }


}
