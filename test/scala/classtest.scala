import java.io.File

import upem.Services

object classtest extends App
{
  override def main(args: Array[String]): Unit = {
    val file: File = new File("D:\\UPEM-Logiciels\\Scala\\Tondeuse\\src\\test\\resources\\input.txt")
    val p = Services.pelouse(file)
    val com = Services.commande(file)
    val posi = Services.position(file)

    posi.zipWithIndex.foreach {
      case (pos, index) if (com.length >= index + 1) => {
        val moved = com(index).foldLeft(pos)((acc, c) => Services.move(acc, c))
        println(pos)
      }
      case (pos, index) => println(pos)
    }
  }
}

