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
  val itsCurso100 = itoObj.itinerariosSalida(vuelosC1, aeropuertos)

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
    val its4 = itsCurso15("HOU", "BNA", 14,20)
    assert(its4 ===List(Vuelo("4X", 373, "HOU", 13, 15, "MSY", 15, 10, 1), Vuelo("AA", 828, "MSY", 17, 10, "BNA", 18, 37, 0)) )
  }

  test("test5") {
    val its5 = itsCurso40("DFW", "ATL",12,16)
    assert(its5 === List(Vuelo("AA", 864, "DFW", 6, 56, "ATL", 15, 3, 0)))
  }

  test("test6") {
    val its6 = itsCurso100("PHX", "LAX",20,34)
    assert(its6 === List(Vuelo("DL", 296, "PHX", 14, 25, "ATL", 19, 53, 0), Vuelo("DL", 842, "ATL", 19, 1, "PHL", 10, 58, 0), Vuelo("DL", 389, "PHL", 19, 35, "TPA", 13, 45, 1), Vuelo("DL", 141, "TPA", 17, 55, "LAX", 19, 53, 0)) )
  }

  test("test9") {
    val its9 = itsCurso100("PHX", "DTW",2,32)
    assert(its9 === List(Vuelo("DL", 296, "PHX", 14, 25, "ATL", 19, 53, 0), Vuelo("DL", 714, "ATL", 23, 47, "DTW", 12, 5, 0)))
  }
}
