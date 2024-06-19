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
class TestPruebasSeq1 extends AnyFunSuite {

  val itoObj = new Itinerario()
  val itsCurso = itoObj.itinerarios(vuelosCurso, aeropuertosCurso)
  val itsCurso15 = itoObj.itinerarios(vuelosA1, aeropuertos)
  val itsCurso40 = itoObj.itinerarios(vuelosB1, aeropuertos)
  val itsCurso100 = itoObj.itinerarios(vuelosC1, aeropuertos)

  test("test1") {
    val its1 = itsCurso("MID", "SVCS")
    assert(its1 === List(List(Vuelo("AIRVZLA", 601, "MID", 5, 0, "SVCS", 6, 0, 0))))
  }

  test("test2") {
    val its2 = itsCurso("CLO", "SVCS")
    assert(its2 === List())
  }

  test("test3") {
    val its3 = itsCurso("CLO", "SVO")
    assert(its3 === List(
      List(Vuelo("AVA", 9432, "CLO", 7, 0, "SVO", 2, 20, 4)),
      List(Vuelo("AVA", 9432, "CLO", 7, 0, "BOG", 8, 0, 0),
        Vuelo("IBERIA", 505, "BOG", 18, 0, "MAD", 12, 0, 0),
        Vuelo("IBERIA", 506, "MAD", 14, 0, "SVO", 23, 20, 0)),
      List(Vuelo("AVA", 9432, "CLO", 7, 0, "BOG", 8, 0, 0),
        Vuelo("IBERIA", 505, "BOG", 18, 0, "MAD", 12, 0, 0),
        Vuelo("IBERIA", 507, "MAD", 16, 0, "SVO", 1, 20, 0)),
      List(Vuelo("TURKISH", 7799, "CLO", 7, 0, "IST", 14, 0, 3),
        Vuelo("QATAR", 5566, "IST", 23, 0, "SVO", 2, 0, 0))
    ))
  }

  test("test4") {
    val its4 = itsCurso("CLO", "MEX")
    assert(its4 === List(
      List(Vuelo("AVA", 9432, "CLO", 7, 0, "BOG", 8, 0, 0),
        Vuelo("LATAM", 787, "BOG", 17, 0, "MEX", 19, 0, 0)),
      List(Vuelo("AVA", 9432, "CLO", 7, 0, "BOG", 8, 0, 0),
        Vuelo("VIVA", 756, "BOG", 9, 0, "MDE", 10, 0, 0),
        Vuelo("VIVA", 769, "MDE", 11, 0, "BAQ", 12, 0, 0),
        Vuelo("AVA", 5643, "BAQ", 14, 0, "MEX", 16, 0, 0))))
  }

  test("test5") {
    val its5 = itsCurso("CTG", "PTY")
    assert(its5 === List(
      List(Vuelo("COPA", 1234, "CTG", 10, 0, "PTY", 11, 30, 0)),
      List(Vuelo("AVA", 4321, "CTG", 9, 30, "SMR", 10, 0, 0), Vuelo("COPA", 7631, "SMR", 10, 50, "PTY", 11, 50, 0))))
  }

  test("test6") {
    val its6 = itsCurso15("HOU", "BNA")
    assert(its6 === List(List(Vuelo("4X", 373, "HOU", 13, 15, "MSY", 15, 10, 1), Vuelo("AA", 828, "MSY", 17, 10, "BNA", 18, 37, 0)), List(Vuelo("4X", 216, "HOU", 14, 30, "MSY", 18, 10, 3), Vuelo("AA", 828, "MSY", 17, 10, "BNA", 18, 37, 0)), List(Vuelo("4X", 375, "HOU", 18, 20, "MSY", 10, 15, 1), Vuelo("AA", 828, "MSY", 17, 10, "BNA", 18, 37, 0)), List(Vuelo("4X", 214, "HOU", 9, 0, "MSY", 12, 40, 3), Vuelo("AA", 828, "MSY", 17, 10, "BNA", 18, 37, 0)), List(Vuelo("4X", 371, "HOU", 8, 35, "MSY", 10, 30, 1), Vuelo("AA", 828, "MSY", 17, 10, "BNA", 18, 37, 0))))
  }

  test("test7") {
    val its7 = itsCurso40("DFW", "ATL")
    assert(its7 == List(List(Vuelo("AA", 864, "DFW", 6, 56, "ATL", 15, 3, 0)), List(Vuelo("AA", 717, "DFW", 20, 0, "LAX", 11, 7, 0), Vuelo("AA", 350, "LAX", 1, 50, "DCA", 11, 46, 1), Vuelo("AA", 319, "DCA", 15, 59, "ORD", 17, 10, 0), Vuelo("AA", 180, "ORD", 10, 14, "ATL", 13, 4, 0)), List(Vuelo("AA", 834, "DFW", 20, 0, "DCA", 13, 36, 0), Vuelo("AA", 319, "DCA", 15, 59, "ORD", 17, 10, 0), Vuelo("AA", 180, "ORD", 10, 14, "ATL", 13, 4, 0)), List(Vuelo("AA", 333, "DFW", 11, 20, "LAX", 12, 19, 0), Vuelo("AA", 350, "LAX", 1, 50, "DCA", 11, 46, 1), Vuelo("AA", 319, "DCA", 15, 59, "ORD", 17, 10, 0), Vuelo("AA", 180, "ORD", 10, 14, "ATL", 13, 4, 0)), List(Vuelo("AA", 654, "DFW", 20, 6, "MSY", 11, 33, 0), Vuelo("AA", 756, "MSY", 11, 58, "BNA", 13, 20, 0), Vuelo("AA", 687, "BNA", 14, 5, "MIA", 17, 25, 0), Vuelo("AA", 960, "MIA", 19, 7, "ORD", 11, 33, 0), Vuelo("AA", 180, "ORD", 10, 14, "ATL", 13, 4, 0)), List(Vuelo("AA", 654, "DFW", 20, 6, "MSY", 11, 33, 0), Vuelo("AA", 756, "MSY", 11, 58, "BNA", 13, 20, 0), Vuelo("AA", 687, "BNA", 14, 5, "MIA", 17, 25, 0), Vuelo("AA", 976, "MIA", 13, 40, "ORD", 15, 55, 0), Vuelo("AA", 180, "ORD", 10, 14, "ATL", 13, 4, 0)), List(Vuelo("AA", 654, "DFW", 20, 6, "MSY", 11, 33, 0), Vuelo("AA", 756, "MSY", 11, 58, "BNA", 13, 20, 0), Vuelo("AA", 687, "BNA", 14, 5, "MIA", 17, 25, 0), Vuelo("AA", 384, "MIA", 15, 24, "ORD", 17, 33, 0), Vuelo("AA", 180, "ORD", 10, 14, "ATL", 13, 4, 0)), List(Vuelo("AA", 654, "DFW", 20, 6, "MSY", 11, 33, 0), Vuelo("AA", 756, "MSY", 11, 58, "BNA", 13, 20, 0), Vuelo("AA", 283, "BNA", 19, 38, "ORD", 11, 16, 0), Vuelo("AA", 180, "ORD", 10, 14, "ATL", 13, 4, 0)), List(Vuelo("AA", 616, "DFW", 13, 2, "MSY", 14, 32, 0), Vuelo("AA", 756, "MSY", 11, 58, "BNA", 13, 20, 0), Vuelo("AA", 687, "BNA", 14, 5, "MIA", 17, 25, 0), Vuelo("AA", 960, "MIA", 19, 7, "ORD", 11, 33, 0), Vuelo("AA", 180, "ORD", 10, 14, "ATL", 13, 4, 0)), List(Vuelo("AA", 616, "DFW", 13, 2, "MSY", 14, 32, 0), Vuelo("AA", 756, "MSY", 11, 58, "BNA", 13, 20, 0), Vuelo("AA", 687, "BNA", 14, 5, "MIA", 17, 25, 0), Vuelo("AA", 976, "MIA", 13, 40, "ORD", 15, 55, 0), Vuelo("AA", 180, "ORD", 10, 14, "ATL", 13, 4, 0)), List(Vuelo("AA", 616, "DFW", 13, 2, "MSY", 14, 32, 0), Vuelo("AA", 756, "MSY", 11, 58, "BNA", 13, 20, 0), Vuelo("AA", 687, "BNA", 14, 5, "MIA", 17, 25, 0), Vuelo("AA", 384, "MIA", 15, 24, "ORD", 17, 33, 0), Vuelo("AA", 180, "ORD", 10, 14, "ATL", 13, 4, 0)), List(Vuelo("AA", 616, "DFW", 13, 2, "MSY", 14, 32, 0), Vuelo("AA", 756, "MSY", 11, 58, "BNA", 13, 20, 0), Vuelo("AA", 283, "BNA", 19, 38, "ORD", 11, 16, 0), Vuelo("AA", 180, "ORD", 10, 14, "ATL", 13, 4, 0)), List(Vuelo("AA", 498, "DFW", 14, 25, "MIA", 18, 4, 0), Vuelo("AA", 960, "MIA", 19, 7, "ORD", 11, 33, 0), Vuelo("AA", 180, "ORD", 10, 14, "ATL", 13, 4, 0)), List(Vuelo("AA", 498, "DFW", 14, 25, "MIA", 18, 4, 0), Vuelo("AA", 976, "MIA", 13, 40, "ORD", 15, 55, 0), Vuelo("AA", 180, "ORD", 10, 14, "ATL", 13, 4, 0)), List(Vuelo("AA", 498, "DFW", 14, 25, "MIA", 18, 4, 0), Vuelo("AA", 397, "MIA", 13, 10, "BNA", 14, 29, 0), Vuelo("AA", 283, "BNA", 19, 38, "ORD", 11, 16, 0), Vuelo("AA", 180, "ORD", 10, 14, "ATL", 13, 4, 0)), List(Vuelo("AA", 498, "DFW", 14, 25, "MIA", 18, 4, 0), Vuelo("AA", 384, "MIA", 15, 24, "ORD", 17, 33, 0), Vuelo("AA", 180, "ORD", 10, 14, "ATL", 13, 4, 0))))
  }

  test("test8") {
    val its8 = itsCurso100("PHX", "LAX")
    assert(its8.length == 3042)
  }

  test("test9") {
    val its9 = itsCurso100("PHX", "DTW")
    assert(its9.length == 4080)
  }


}