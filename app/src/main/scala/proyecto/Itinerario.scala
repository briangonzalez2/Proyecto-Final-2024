package proyecto

class Itinerario() {

  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

  def tamanio(lista: List[Vuelo], acc: Int): Int = {
    if (lista.isEmpty) {
      acc
    } else {
      tamanio(lista.tail, acc + 1)
    }
  }

  def buscarVuelos(origen: String, destino: String, vuelosDisponibles: List[Vuelo]): List[List[Vuelo]] = {
    def buscar(origen: String, destino: String, vuelosDisponibles: List[Vuelo], itinerarioActual: List[Vuelo]): List[List[Vuelo]] = {
      if (origen == destino) List(itinerarioActual)
      else {
        vuelosDisponibles.collect {
          case vuelo if vuelo.Org == origen =>
            buscar(vuelo.Dst, destino, vuelosDisponibles.filterNot(_ == vuelo), itinerarioActual :+ vuelo)
        }.flatten
      }
    }

    buscar(origen, destino, vuelosDisponibles, List.empty[Vuelo])
  }

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {

    val vuelosOrigen = vuelos.groupBy(_.Org)

    def buscarItinerarios(codigoOrigen: String, codigoDestino: String, visitados: Set[String]): List[List[Vuelo]] = {
      if (codigoOrigen == codigoDestino) {
        List(List())
      } else {
        val vuelosDisp = vuelosOrigen.getOrElse(codigoOrigen, List())
        vuelosDisp.flatMap { vuelo =>
          if (!visitados.contains(vuelo.Dst)) {
            val itinerariosRestantes = buscarItinerarios(vuelo.Dst, codigoDestino, visitados + vuelo.Org)
            itinerariosRestantes.map(itinerario => vuelo :: itinerario)
          } else {
            List()
          }
        }
      }
    }

    (cod1: String, cod2: String) => buscarItinerarios(cod1, cod2, Set())
  }


  def calcularVuelo(vuelo: Vuelo, aeropuertos: List[Aeropuerto]): Double = {
    val origen = aeropuertos.find(_.Cod == vuelo.Org).get
    val destino = aeropuertos.find(_.Cod == vuelo.Dst).get
    val diferenciaHoraria = (destino.GMT - origen.GMT) / 100

    val salidaMinutos = vuelo.HS * 60 + vuelo.MS
    val llegadaMinutos = vuelo.HL * 60 + vuelo.ML + diferenciaHoraria * 60

    if (llegadaMinutos >= salidaMinutos) llegadaMinutos - salidaMinutos
    else llegadaMinutos + (24 * 60) - salidaMinutos
  }

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def buscarItinerarios(origen: String, destino: String, vuelosDisponibles: List[Vuelo]): List[List[Vuelo]] = {
      def buscar(origen: String, destino: String, vuelosDisponibles: List[Vuelo], itinerarioActual: List[Vuelo]): List[List[Vuelo]] = {
        if (origen == destino) List(itinerarioActual)
        else {
          vuelosDisponibles.collect {
            case vuelo if vuelo.Org == origen =>
              buscar(vuelo.Dst, destino, vuelosDisponibles.filterNot(_ == vuelo), itinerarioActual :+ vuelo)
          }.flatten
        }
      }

      buscar(origen, destino, vuelosDisponibles, List.empty[Vuelo])
    }

    (cod1: String, cod2: String) => {
      val itinerariosPosibles = buscarItinerarios(cod1, cod2, vuelos)
      itinerariosPosibles.sortBy(_.map(calcularVuelo(_, aeropuertos)).sum).take(3)
    }
  }

  def calcularEscalas(itinerario: List[Vuelo]): Int = {
    itinerario.map(_.Esc).sum
  }

  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    def buscarItinerarios(origen: String, destino: String, vuelosDisponibles: List[Vuelo]): List[List[Vuelo]] = {
      def buscar(origen: String, destino: String, vuelosDisponibles: List[Vuelo], itinerarioActual: List[Vuelo]): List[List[Vuelo]] = {
        if (origen == destino) List(itinerarioActual)
        else {
          vuelosDisponibles.collect {
            case vuelo if vuelo.Org == origen =>
              buscar(vuelo.Dst, destino, vuelosDisponibles.filterNot(_ == vuelo), itinerarioActual :+ vuelo)
          }.flatten
        }
      }

      buscar(origen, destino, vuelosDisponibles, List.empty[Vuelo])
    }

    (cod1: String, cod2: String) => {
      val itinerariosPosibles = buscarItinerarios(cod1, cod2, vuelos)
      itinerariosPosibles.sortBy(calcularEscalas).take(3)
    }
  }


  def calcularTiempoVuelo(vuelo: Vuelo, aeropuertos: List[Aeropuerto]): Double = {
    val origen = aeropuertos.find(_.Cod == vuelo.Org).get
    val destino = aeropuertos.find(_.Cod == vuelo.Dst).get
    val diferenciaHoraria = (destino.GMT - origen.GMT) / 100

    val salida = vuelo.HS * 60 + vuelo.MS
    val llegada = vuelo.HL * 60 + vuelo.ML + diferenciaHoraria * 60
    if (llegada >= salida) llegada - salida else (llegada + 24 * 60) - salida
  }

  def tiempoTotalVuelo(itinerario: List[Vuelo], aeropuertos: List[Aeropuerto]): Double = {
    itinerario.map(vuelo => calcularTiempoVuelo(vuelo, aeropuertos)).sum
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    (cod1: String, cod2: String) => {
      val itinerariosPosibles = buscarVuelos(cod1, cod2, vuelos)
      itinerariosPosibles.sortBy(ti => tiempoTotalVuelo(ti, aeropuertos)).take(3)
    }
  }


  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[Vuelo] = {
    def calcularTiempo(itinerario: List[Vuelo], horaLlegada: Int, minutoLlegada: Int): Int = {
      val ultimaLlegada = itinerario.last.HL * 60 + itinerario.last.ML
      val llegadaEsperada = horaLlegada * 60 + minutoLlegada
      val diferencia = llegadaEsperada - ultimaLlegada
      if (diferencia >= 0) diferencia else Int.MaxValue
    }

    def obtenerMejorItinerario(codigoOrigen: String, codigoDestino: String, horaLlegada: Int, minutoLlegada: Int): List[Vuelo] = {
      val lista = itinerarios(vuelos, aeropuertos)(codigoOrigen, codigoDestino)
      val itinerarioTiempos = lista.map(itinerario => (itinerario, calcularTiempo(itinerario, horaLlegada, minutoLlegada)))
      val itinerariosValidos = itinerarioTiempos.filter(_._2 < Int.MaxValue).sortBy(_._2)
      itinerariosValidos.map(_._1).headOption.getOrElse(List.empty[Vuelo])
    }

    (cod1: String, cod2: String, horaCita: Int, minutoCita: Int) => obtenerMejorItinerario(cod1, cod2, horaCita, minutoCita)
  }

}





