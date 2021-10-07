import scala.util.Random
import scala.io.Source
import java.io._


object Juego {

  class Limpieza() {

    def limpieza_de_archivo(): Unit = {
      val filename ="/home/juan_ferreyra/Escritorio/Novakorp/Project_Pokemon/src/main/scala/pokemon.csv"
      val newfile = new File("/home/juan_ferreyra/Escritorio/Novakorp/Project_Pokemon/src/main/scala/NUEVO.csv")
      val cursor = new BufferedWriter(new FileWriter(newfile))

      for (line <- Source.fromFile(filename).getLines) {
        //line = algocambiado
        val linea_sep = line.split(",").reverse

        val linea_objective = List(linea_sep(10), linea_sep(8), linea_sep(12), linea_sep(21), linea_sep(15), linea_sep(7), linea_sep(6),
          linea_sep(5), linea_sep(1), linea_sep(0))

        for (x <- linea_objective) {
          cursor.write(s"$x,")
        }
        cursor.write("\n")
      }
      cursor.close()


    } //llave limpieza_de_archivos

  }//llave limpieza


  case class Pokemon(nombre: String, pd_num: String, var hp: Int, atk: Int, df: Int, sp_atk: Int, sp_df: Int, var speed: Int, gen: Int, isleg: Int) {

    //println(s"$nombre, $pd_num, $hp, $atk, $df, $sp_atk, $sp_df, $speed, $gen, $isleg")
  }




  class Pokeball(filename:String) {

    //objeto de la clase pokemon
    var pokemon_atrapado: Pokemon = _

    def atrapar(): Unit ={
      def generar_ale(): Int = {
        var numero = Random.between(1, 801)
        numero
      }
      var n_a = generar_ale() //con esto hago que cuando llamo a atrapar me llame al metodo generar_ale y me lo guarde en la variable n_a

      var c = 0
      var linea_encontrada = ""
      var car = new Array[String](500)
      for (line <- Source.fromFile(filename).getLines) {
        c = c + 1
        if (c == n_a) {
          linea_encontrada = line
          car = linea_encontrada.split(",")
        }
      }
      pokemon_atrapado = new Juego.Pokemon(car(0),car(1),car(2).toInt,car(3).toInt,car(4).toInt,car(5).toInt,car(6).toInt,car(7).toInt,car(8).toInt, car(9).toInt)
    }
  }//llave pokeball class





  def main(args: Array[String]) = {
    //llamamos a las clases
    val limpiador = new Limpieza
    limpiador.limpieza_de_archivo()
    val pokeball_x = new Pokeball("/home/juan_ferreyra/Escritorio/Novakorp/Project_Pokemon/src/main/scala/NUEVO.csv")
    pokeball_x.atrapar()
  }

}//llave objeto


