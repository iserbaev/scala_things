package tasks

object Main10 {
  import java.io.{BufferedReader, InputStreamReader}
  import java.util.StringTokenizer
  import scala.collection.mutable
  import scala.collection.mutable.ArrayBuffer

  sealed trait Command {
    def cmd:      String
    def args:     Array[String]
    def validate: Boolean
    def run(store: mutable.Map[Int, String]): mutable.Map[Int, String]
    def run(store: Array[String]):            Unit
  }
  object Command {
    case class Add(args: Array[String]) extends Command {
      def validate: Boolean = args.length == 3
      def run(store: mutable.Map[Int, String]): mutable.Map[Int, String] =
        store.+((num.toInt, name))

      def run(store: Array[String]): Unit =
        store.update(num.toInt, name)

      val cmd:  String = "add"
      val num:  String = args(1)
      val name: String = args(2)
    }
    case class Find(args: Array[String]) extends Command {
      def validate: Boolean = args.length == 2

      def run(store: mutable.Map[Int, String]): mutable.Map[Int, String] = {
        println(store.getOrElse(num.toInt, "not found"))
        store
      }

      def run(store: Array[String]): Unit =
        Option(store.apply(num.toInt)).getOrElse("not found")

      val cmd: String = "find"
      val num: String = args(1)
    }
    case class Del(args: Array[String]) extends Command {
      def validate: Boolean = args.length == 2

      def run(store: mutable.Map[Int, String]): mutable.Map[Int, String] =
        store.-(num.toInt)

      def run(store: Array[String]): Unit =
        store.update(num.toInt, null)

      val cmd: String = "find"
      val num: String = args(1)
    }

    val mapping = Map(
      "add"  -> ((args: Array[String]) => Add(args)),
      "find" -> ((args: Array[String]) => Find(args)),
      "del"  -> ((args: Array[String]) => Del(args))
    )

    def parse(str: String): Option[Command] = {
      val args: Array[String] = str.split(" ")

      mapping
        .get(args(0))
        .map(_.apply(args))
    }
  }

  def main(args: Array[String]) = {
    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    val n = br.read()

    (1 to n)
      .flatMap(_ => Option(br.readLine()))
      .flatMap(Command.parse)
      .foldLeft(mutable.Map.empty[Int, String]) {
        case (acc, command) =>
          command.run(acc)
      }

    br.close()
  }

  def process(arr: Array[String]) =
    arr
      .flatMap(Command.parse)
      .foldLeft(mutable.Map.empty[Int, String]) {
        case (acc, command) =>
          command.run(acc)
      }

}

object Test10 extends App {
  val cmds = Seq(
    "add 911 police",
    "add 76213 Mom",
    "add 17239 Bob",
    "find 76213",
    "find 910",
    "find 911",
    "del 910",
    "del 911",
    "find 911",
    "find 76213",
    "add 76213 daddy",
    "find 76213"
  )

  Main10.process(cmds.toArray)
}
