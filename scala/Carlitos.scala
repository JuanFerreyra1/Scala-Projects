import sun.audio._
import java.io._
import scala.util.Random
import Juego.Pokemon

object Carlitos {

  class Entrenador(nombre: String) {

    val nombre_entrenador: String = nombre
    var lista_pokemones: Array[Pokemon] = Array[Juego.Pokemon]()
    var cantidad_pokemones = 0

    def agregar_pokemon(poke: Pokemon): Unit = {
      lista_pokemones = lista_pokemones :+ poke
      this.cantidad_pokemones += 1
    }

    def eliminar_pokemon(): Unit = {
      //ARREGLO: VER CUAL ELEMENTO ELIMINA
      this.lista_pokemones = this.lista_pokemones.drop(1)
      cantidad_pokemones -= 1
      print(s"murio un pokemon de $nombre_entrenador \n")

      if (this.lista_pokemones.length == 0) {
        print(s"Su lista quedó vacia")
      }
      else {
        print(s"Su lista quedó:")
      }
      for (pokemon <- lista_pokemones){
        print(s" ${pokemon.nombre}, ")
      }
      print("\n")
    }
  }

  class SpeedKeeper(){
    var velocidades = Map[String,Int]()

    def add_speed_pok(pokemon: Pokemon): Unit ={
      velocidades += (pokemon.nombre->pokemon.speed)
    }

    def reestablecer_velocidades(lista: Array[Pokemon]): Unit={
      for (pokemon <- lista){
        pokemon.speed = velocidades(pokemon.nombre)
      }
    }
  }



  class Estadio(juan: Carlitos.Entrenador, peti: Carlitos.Entrenador, lore: Carlitos.Entrenador) {

    var lista_entrenadores: Array[Entrenador] = Array(juan,peti,lore)


    def buscar_pokemon(entrenador: Carlitos.Entrenador, velocidades: SpeedKeeper): Unit = {

      val limpiador = new Juego.Limpieza
      limpiador.limpieza_de_archivo()
      for (i <- 0 until 3) {
        val pokeball_x = new Juego.Pokeball("/home/juan_ferreyra/Escritorio/Novakorp/Project_Pokemon/src/main/scala/NUEVO.csv") // creamos objeto pokeball
        pokeball_x.atrapar()

        val pokemon_encontrado = pokeball_x.pokemon_atrapado //objeto pokemon que encontramos
        entrenador.agregar_pokemon(pokemon_encontrado)
        velocidades.add_speed_pok(pokemon_encontrado)
      }
    }

    def determinar_peleadores(lista_entrenadores: Array[Entrenador]): Array[Pokemon] = {
      val cant_iteraciones = lista_entrenadores.length
      var lista_peleadores: Array[Pokemon] = Array()
      for (i <- 0 until cant_iteraciones){

        if (cant_iteraciones ==2) {

          //si murieron todos los pokemones de juan
          if (lista_entrenadores(0) == peti){
              //ARREGLO: Por lo que pasa en eliminar muerto(filter): peti queda 0 y lore 1 en lista entrenadores
               val entrenador = lista_entrenadores(0+i)
              //ARREGLO: VER QUE ELEMENTO ELIMINA eliminar_pokemon
               val pokemon = entrenador.lista_pokemones(0)
               lista_peleadores = lista_peleadores :+ pokemon
              }

          //si murieron todos los pokemones de peti
          if (lista_entrenadores(0) == juan){
            if (lista_entrenadores(1) == lore){
              //ARREGLO: Por lo que pasa en eliminar muerto(filter): juan queda 0 y lore 1 en lista entrenadores
              val entrenador = lista_entrenadores(0+i)
              //ARREGLO: VER QUE ELEMENTO ELIMINA eliminar_pokemon
              val pokemon = entrenador.lista_pokemones(0)
              lista_peleadores = lista_peleadores :+ pokemon
            }

          }
          //si murieron todos los pokemones de lore
          if (lista_entrenadores(0) == juan) {
                if (lista_entrenadores(1) == peti) {
                  //ARREGLO: Por lo que pasa en eliminar muerto(filter): juan queda 0 y peti 1 en lista entrenadores
                  val entrenador = lista_entrenadores(0+i)
                  //ARREGLO: VER QUE ELEMENTO ELIMINA eliminar_pokemon
                  val pokemon = entrenador.lista_pokemones(0)
                  lista_peleadores = lista_peleadores :+ pokemon
                 }
             }
             }

        else {
          val entrenador = lista_entrenadores(i)
          val pokemon = entrenador.lista_pokemones(0)
          lista_peleadores = lista_peleadores :+ pokemon
        }

      } //for
      lista_peleadores
    }

    def determinar_frecuencia(pokemones: Array[Juego.Pokemon]): Int = {
      val cant_iteraciones = pokemones.length - 1
      var minimo = 9999
      for (i <- 0 to cant_iteraciones){
        val pokemon = pokemones(i)
        if (pokemon.speed < minimo) minimo = pokemon.speed
      }
      minimo
    }

    def obtener_defensores(nombre_atacante: String, diccionario: Map[Int, Juego.Pokemon]): Array[Pokemon] ={
      var lista_defensores = Array[Pokemon]()
      for (elemento <- diccionario){
        if (elemento._2.nombre != nombre_atacante) lista_defensores = lista_defensores :+ elemento._2
      }

      lista_defensores
    }

    def ataque(atacante: Pokemon, lista_defensores: Array[Pokemon]): Unit ={
      var indice_defensor = 0

      if (lista_defensores.length == 1){
        var indice_defensor = 0
        val defensor = lista_defensores(indice_defensor)
        val ad = atacante.atk - defensor.df
        val ap = atacante.sp_atk - defensor.sp_df
        var esperanza_ataque = List(ad, ap).max
        if (esperanza_ataque <= 0) {esperanza_ataque = 1}
        defensor.hp = defensor.hp - esperanza_ataque
        print(s"\n${atacante.nombre} esta realizando un ataque a ${defensor.nombre} ...\n")
        Thread.sleep(2000)
        print(s"${atacante.nombre} atacó a ${defensor.nombre} quitándole $esperanza_ataque de vida, y dejándolo con ${defensor.hp}HP\n")
      }

      if (lista_defensores.length == 2){
        indice_defensor = Random.between(0, 2)
        val defensor = lista_defensores(indice_defensor)
        val ad = atacante.atk - defensor.df
        val ap = atacante.sp_atk - defensor.sp_df
        var esperanza_ataque = List(ad, ap).max
        if (esperanza_ataque <= 0) {esperanza_ataque = 1}
        defensor.hp = defensor.hp - esperanza_ataque
        print(s"\n${atacante.nombre} esta realizando un ataque a ${defensor.nombre} ...\n")
        Thread.sleep(2000)
        print(s"${atacante.nombre} atacó a ${defensor.nombre} quitándole $esperanza_ataque de vida, y dejándolo con ${defensor.hp}HP\n")
      }

    }

    def eliminar_muerto(muerto: Pokemon, velocidades: SpeedKeeper): Unit ={
      for (entrenador <- lista_entrenadores){
        if (entrenador.lista_pokemones(0) == muerto) {
          entrenador.eliminar_pokemon()
        }
        if (entrenador.lista_pokemones.length == 0) lista_entrenadores = lista_entrenadores.filter(x => x.lista_pokemones.length != 0)
      }
      val lista_peleadores = determinar_peleadores(lista_entrenadores)
      velocidades.reestablecer_velocidades(lista_peleadores)
      val frecuencia = determinar_frecuencia(lista_peleadores)
      recursion(lista_entrenadores, frecuencia, velocidades)
    }


    def recursion(lista_entrenadores: Array[Entrenador], frecuencia: Int, velocidades: SpeedKeeper): Entrenador = {


      //print(s"NUEVA RECURSION: ${lista_entrenadores.length}\n")
      if (lista_entrenadores.length == 1){
        //print("HAY UN GANADOR\n\n")
        return lista_entrenadores(0)
      }

      //ARREGLO HECHO: en determinar_peleadores
      val lista_peleadores = determinar_peleadores(lista_entrenadores)
      var prioridades = Map(1->lista_peleadores(0))
      var velocidad_atacante: Int = 0
      var lista_defensores = Array[Pokemon]()
      var atacante: Pokemon = lista_peleadores(0)


      if (lista_peleadores.length == 2) {

        //ARREGLO 1: SI EL ORDEN DE DETERMINAR PELEADORES SE MANTIENE
        //si juan sin pokemones
          if (lista_entrenadores(0) == peti) {
            val pok_1 = lista_peleadores(0); val pok_2 = lista_peleadores(1)
            val velocidad_1 = pok_1.speed; val velocidad_2 = pok_2.speed
            //val velocidades = Map(pok_1 -> velocidad_1, pok_2 -> velocidad_2)
            val prioridades2 = Map(velocidad_1 -> pok_1, velocidad_2 -> pok_2)
            prioridades = prioridades2
            val velocidad_atacante_2 = List(velocidad_1, velocidad_2).max
            velocidad_atacante = velocidad_atacante_2
            atacante = prioridades(velocidad_atacante)
            lista_defensores = obtener_defensores(atacante.nombre, prioridades)
          }

        //si murieron todos los pokemones de peti
          if (lista_entrenadores(0) == juan){
            val pok_1 = lista_peleadores(0); val pok_2 = lista_peleadores(1)
            val velocidad_1 = pok_1.speed; val velocidad_2 = pok_2.speed
            //val velocidades = Map(pok_1 -> velocidad_1, pok_2 -> velocidad_2)
            val prioridades2 = Map(velocidad_1 -> pok_1, velocidad_2 -> pok_2)
            prioridades = prioridades2
            val velocidad_atacante_2 = List(velocidad_1, velocidad_2).max
            velocidad_atacante = velocidad_atacante_2
            atacante = prioridades(velocidad_atacante)
            lista_defensores = obtener_defensores(atacante.nombre, prioridades)
          }

        //si murieron todos los pokemones de lore
          if (lista_entrenadores(0) == juan) {
                if (lista_entrenadores(1) == peti) {
                    val pok_1 = lista_peleadores(0); val pok_2 = lista_peleadores(1)
                    val velocidad_1 = pok_1.speed; val velocidad_2 = pok_2.speed
                    //val velocidades = Map(pok_1 -> velocidad_1, pok_2 -> velocidad_2)
                    val prioridades2 = Map(velocidad_1 -> pok_1, velocidad_2 -> pok_2)
                    prioridades = prioridades2
                    val velocidad_atacante_2 = List(velocidad_1, velocidad_2).max
                    velocidad_atacante = velocidad_atacante_2
                    atacante = prioridades(velocidad_atacante)
                    lista_defensores = obtener_defensores(atacante.nombre, prioridades)
                } }


      }
      else if (lista_peleadores.length == 3){
        val pok_1 = lista_peleadores(0); val pok_2 = lista_peleadores(1); val pok_3 = lista_peleadores(2)
        val velocidad_1 = pok_1.speed; val velocidad_2 = pok_2.speed; val velocidad_3 = pok_3.speed

        //val velocidades = Map(pok_1 -> velocidad_1, pok_2 -> velocidad_2, pok_3 -> velocidad_3 )
        val prioridades3 = Map(velocidad_1 -> pok_1, velocidad_2 -> pok_2, velocidad_3 -> pok_3)
        prioridades = prioridades3
        val velocidad_atacante_3 = List(velocidad_1, velocidad_2, velocidad_3).max
        velocidad_atacante = velocidad_atacante_3
        atacante = prioridades(velocidad_atacante)
        lista_defensores = obtener_defensores(atacante.nombre, prioridades)
      }
      else {
        lista_peleadores(0)
      }



      ataque(atacante, lista_defensores)
      atacante.speed = atacante.speed - frecuencia


      // eliminacion del pokemon
      for (pokemon <-  lista_peleadores)
        if (pokemon.hp <= 0)
          eliminar_muerto(pokemon, velocidades)
      recursion(this.lista_entrenadores, frecuencia, velocidades)
    }
  }



  def main(args: Array[String]): Unit = {
    val file_fetched = new FileInputStream("/home/juan_ferreyra/Escritorio/Novakorp/Project_Pokemon/src/main/scala/musica_batalla_2.wav");
    val execute_music_fetched = new AudioStream(file_fetched);
    AudioPlayer.player.start(execute_music_fetched);
    val juan = new Entrenador("Juan"); val peti = new Entrenador("Peti"); val lore = new Entrenador("Lore")
    val estadio_azulona = new Estadio(juan, peti, lore)
    val velocidades = new SpeedKeeper
    estadio_azulona.buscar_pokemon(juan,velocidades)
    estadio_azulona.buscar_pokemon(peti,velocidades)
    estadio_azulona.buscar_pokemon(lore,velocidades)
    val lista_peleadores = estadio_azulona.determinar_peleadores(estadio_azulona.lista_entrenadores)
    val frecuencia = estadio_azulona.determinar_frecuencia(lista_peleadores)
    print("\n                                                ¡LA BATALLA ESTA POR COMENZAR!   \n")
    Thread.sleep(2000)
    val ganador = estadio_azulona.recursion(estadio_azulona.lista_entrenadores, frecuencia, velocidades)
    AudioPlayer.player.stop(execute_music_fetched)
    print("\n                                                   ¡LA BATALLA HA FINALIZADO!\n")
    val file_fetched2 = new FileInputStream("/home/juan_ferreyra/Escritorio/Novakorp/Project_Pokemon/src/main/scala/GANADOR.wav");
    val execute_music_fetched2 = new AudioStream(file_fetched2);
    AudioPlayer.player.start(execute_music_fetched2);
    Thread.sleep(6000)
    val result = ganador.nombre_entrenador
    print(s"                                            ¡EL ENTRENADOR $result GANO LA BATALLA!")
  }
}