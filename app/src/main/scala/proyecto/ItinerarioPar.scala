package proyecto

import common._
import org.scalameter

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq

import scala.collection.parallel.CollectionConverters._
class ItinerarioPar {

  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

  def tamanio(lista: List[Vuelo], acc: Int): Int = {
    if (lista.isEmpty) {
      acc
    } else {
      tamanio(lista.tail, acc + 1)
    }
  }

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {

    val vuelosOrigen = vuelos.groupBy(_.Org).par

    def buscarItinerarios(codigoOrigen: String, codigoDestino: String, visitados: Set[String]): List[List[Vuelo]] = {
      if (codigoOrigen == codigoDestino) {
        List(List())
      } else {
        val vuelosDisp = vuelosOrigen.getOrElse(codigoOrigen, List()).par
        vuelosDisp.flatMap { vuelo =>
          if (!visitados.contains(vuelo.Dst)) {
            val itinerariosRestantes = buscarItinerarios(vuelo.Dst, codigoDestino, visitados + vuelo.Org)
            itinerariosRestantes.map(itinerario => vuelo :: itinerario)
          } else {
            List()
          }
        }.toList
      }
    }

    (cod1: String, cod2: String) => buscarItinerarios(cod1, cod2, Set())
  }


  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    val zonaHorariaAeropuerto = aeropuertos.map(a => a.Cod -> a.GMT).toMap

    def tiempoTotal(itinerario: List[Vuelo]): Int = {
      itinerario.foldLeft(0) { (acc, vuelo) =>
        val salida = cambioZonaHoraria(vuelo.Org, vuelo.HS + vuelo.MS / 100)
        val llegada = cambioZonaHoraria(vuelo.Dst, vuelo.HL + vuelo.ML / 100)
        val total = if (llegada >= salida) llegada - salida else (24 + llegada) - salida
        acc + total
      }
    }

    def auxItinerarioTiempo(codigoOrigen: String, codigoDestino: String): List[List[Vuelo]] = {
      val lista = itinerarios(vuelos, aeropuertos)(codigoOrigen, codigoDestino).par
      val tiempos = lista.map(tiempoTotal)
      val vuelostiempo = lista.zip(tiempos)
      val vuelosOrdenados = vuelostiempo.toList.sortBy(_._2)
      vuelosOrdenados.map(_._1).take(3)
    }

    def cambioZonaHoraria(codigo: String, hora: Int): Int = {
      zonaHorariaAeropuerto.getOrElse(codigo, 0) match {
        case -400 => hora + 4 case -600 => hora
        case -700 => hora case -800 => hora case 900 => hora + 9
        case -900 => hora case 100 => hora + 1
        case 300 => hora + 3 case 400 => hora + 4 case -500 => hora
      }
    }

    (cod1: String, cod2: String) => auxItinerarioTiempo(cod1, cod2)
  }

  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {

    def escalas(itinerario: List[Vuelo]): Int = {
      itinerario.foldLeft(0) { (acc, vuelo) =>
        acc + vuelo.Esc
      }
    }

    def auxEscalas(codigo1: String, codigo2: String): List[List[Vuelo]] = {
      val lista = itinerarios(vuelos, aeropuertos)(codigo1, codigo2).par
      val numeroEscalas = lista.map(escalas)
      val vueloEscalas = lista.zip(numeroEscalas)
      val vuelosOrdenadosEscalas = vueloEscalas.toList.sortBy(_._2)
      vuelosOrdenadosEscalas.map(_._1).take(3)
    }

    (cod1: String, cod2: String) => auxEscalas(cod1, cod2)
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def obtenerItinerarios(codigoOrigen: String, codigoDestino: String): List[List[Vuelo]] = {
      val todosLosItinerarios = itinerarios(vuelos, aeropuertos)(codigoOrigen, codigoDestino).par
      todosLosItinerarios.take(3).toList
    }
    (cod1: String, cod2: String) => obtenerItinerarios(cod1, cod2)
  }

  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[Vuelo] = {
    def calcularTiempo(itinerario: List[Vuelo], horaLlegada: Int, minutoLlegada: Int): Int = {
      val ultimaLlegada = itinerario.last.HL * 60 + itinerario.last.ML
      val llegadaEsperada = horaLlegada * 60 + minutoLlegada
      val diferencia = llegadaEsperada - ultimaLlegada
      if (diferencia >= 0) diferencia else Int.MaxValue
    }

    def obtenerMejorItinerario(codigoOrigen: String, codigoDestino: String, horaLlegada: Int, minutoLlegada: Int): List[Vuelo] = {
      val lista = itinerarios(vuelos, aeropuertos)(codigoOrigen, codigoDestino).par
      val itinerarioTiempos = lista.map(itinerario => (itinerario, calcularTiempo(itinerario, horaLlegada, minutoLlegada)))
      val itinerariosValidos = itinerarioTiempos.filter(_._2 < Int.MaxValue).toList.sortBy(_._2)
      itinerariosValidos.map(_._1).headOption.getOrElse(List.empty[Vuelo])
    }

    (cod1: String, cod2: String, horaCita: Int, minutoCita: Int) => obtenerMejorItinerario(cod1, cod2, horaCita, minutoCita)
  }


}

