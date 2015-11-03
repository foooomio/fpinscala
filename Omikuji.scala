import scala.util.Random

case class Person(name: String, score: Int) {
    override def toString =
        "%s, score: %s".format(name, score)
}

object Omikuji {
    val members = Array(
        "Atagi",
        "Suzuki",
        "Sodebayashi",
        "Kamigawa",
        "Sasaki",
        "Homma",
        "Murayama",
        "Kanazawa",
        "Toyoda",
        "Kawashima",
        "Yamashita",
        "Oikawa",
        "Usagawa",
        "Nakatsuka"
    )

    def draw(): String = {
        val i = Random.nextInt(members.length)
        members(i)
    }

    def randomScore(max: Int): Int =
        Random.nextInt(max)

    def main(args: Array[String]): Unit = {
        val name = draw()
        println("The next facilitator is " + name + ".")
        println

        val people = members.map{Person(_, randomScore(100))}

        println("More than 60")
        people.filter(_.score >= 60).foreach(println)
        println

        println("Top 3")
        people.sortBy(_.score).reverse.take(3).foreach(println)
    }
}

