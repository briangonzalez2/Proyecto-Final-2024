package proyecto

class Itinerario() {

  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

  type Itinerario = List[Vuelo]

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    def buscarItinerarios(origen: String, destino: String, visitados: Set[String]): List[Itinerario] = {
      if (origen == destino) List(List())
      else {
        val vuelosDesdeOrigen = vuelos.filter(v => v.Org == origen && !visitados.contains(v.Dst))
        vuelosDesdeOrigen.flatMap { vuelo =>
          val subItinerarios = buscarItinerarios(vuelo.Dst, destino, visitados + origen)
          subItinerarios.map(subItinerario => vuelo :: subItinerario)
        }
      }
    }

    (cod1: String, cod2: String) => buscarItinerarios(cod1, cod2, Set())
  }
  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def tiempoTotal(itinerario: List[Vuelo]): Int = {
      itinerario.zipWithIndex.map { case (vuelo, vuelo_indice) =>
        val VueloOrigen = aeropuertos.find(_.Cod == vuelo.Org).get
        val VueloDestino = aeropuertos.find(_.Cod == vuelo.Dst).get
        val DiferenciaHora = (VueloDestino.GMT - VueloOrigen.GMT) / 100

        val Hsalida = vuelo.HS * 60 + vuelo.MS
        val Hllegada = vuelo.HL * 60 + vuelo.ML + (DiferenciaHora * 60).toInt

        if (vuelo_indice != 0) {
          val vueloAnterior = itinerario(vuelo_indice - 1)
          val tiempoTierra = Hsalida - (vueloAnterior.HL * 60 + vueloAnterior.ML)
          if (Hllegada >= Hsalida)
            (Hllegada + tiempoTierra.abs) - Hsalida
          else
            ((Hllegada + tiempoTierra.abs) + (24 * 60)) - Hsalida
        } else {
          if (Hllegada >= Hsalida)
            Hllegada - Hsalida
          else
            (Hllegada + (24 * 60)) - Hsalida
        }
      }.sum
    }

    def encontrarItinerarios(cod1: String, cod2: String): List[List[Vuelo]] = {
      def buscarVuelos(origen: String, destino: String, vuelosDisponibles: List[Vuelo], itinerarioActual: List[Vuelo]): List[List[Vuelo]] = {
        if (origen == destino) List(itinerarioActual)
        else {
          vuelosDisponibles.filter(_.Org == origen).flatMap { vuelo =>
            buscarVuelos(vuelo.Dst, destino, vuelosDisponibles.filterNot(_ == vuelo), itinerarioActual :+ vuelo)
          }
        }
      }
      val itinerariosPosibles = buscarVuelos(cod1, cod2, vuelos, List())
      itinerariosPosibles.sortBy(tiempoTotal).take(3)
    }

    encontrarItinerarios
  }


  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    def buscarItinerariosEscalas(origen: String, destino: String, visitados: Set[String], itinerarioActual: List[Vuelo]): List[Itinerario] = {
      val ultimoVuelo = itinerarioActual.headOption

      if (ultimoVuelo.exists(_.Dst == destino)) {
        List(itinerarioActual.reverse)
      } else {
        val siguienteAeropuerto = ultimoVuelo.map(_.Dst).getOrElse(origen)
        val vuelosPosibles = vuelos.filter(v => v.Org == siguienteAeropuerto && !visitados.contains(v.Dst))

        vuelosPosibles.flatMap { vuelo =>
          buscarItinerariosEscalas(vuelo.Dst, destino, visitados + vuelo.Dst, vuelo :: itinerarioActual)
        }
      }
    }

    (cod1: String, cod2: String) => {
      buscarItinerariosEscalas(cod1, cod2, Set(cod1), Nil).sortBy(_.size).take(3)
    }
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    (cod1: String, cod2: String) => {
      val itinerariosT = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      itinerariosT.sortBy { itin =>
        itin.map(v => (v.HL * 60 + v.ML) - (v.HS * 60 + v.MS)).sum
      }
    }
  }

  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[Itinerario] = {
    (cod1: String, cod2: String, HC: Int, MC: Int) => {
      val ItinerariosT = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      val TiempoCita = HC * 60 + MC
        ItinerariosT.filter { itin =>
        val TiempoLlegada = itin.last.HL * 60 + itin.last.ML
        TiempoLlegada <= TiempoCita
      }.sortBy { itin =>
        val TiempoLlegada = itin.last.HL * 60 + itin.last.ML
        TiempoCita - TiempoLlegada
      }.take(3)
    }
  }
}

