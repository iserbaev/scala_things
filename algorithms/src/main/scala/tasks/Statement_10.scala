package tasks

object Main10 {
  import java.io.{ BufferedReader, InputStreamReader }

  sealed trait Command {
    def cmd: String
    def args: Array[String]
    def validate: Boolean
    def run(store: Map[Int, String]): Map[Int, String]
  }
  object Command {
    case class Add(args: Array[String]) extends Command {
      def validate: Boolean = args.length == 3
      def run(store: Map[Int, String]): Map[Int, String] =
        store.updated(num.toInt, name)

      val cmd: String  = "add"
      val num: String  = args(1)
      val name: String = args(2)
    }
    case class Find(args: Array[String]) extends Command {
      def validate: Boolean = args.length == 2

      def run(store: Map[Int, String]): Map[Int, String] = {
        println(store.getOrElse(num.toInt, "not found"))
        store
      }

      val cmd: String = "find"
      val num: String = args(1)
    }
    case class Del(args: Array[String]) extends Command {
      def validate: Boolean = args.length == 2

      def run(store: Map[Int, String]): Map[Int, String] =
        store.-(num.toInt)

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

    val cmds = (1 to n).flatMap(_ => Option(br.readLine()))

    process(cmds)

    br.close()
  }

  def process(arr: Seq[String]) =
    arr
      .flatMap(Command.parse)
      .foldLeft(Map.empty[Int, String]) { case (acc, command) =>
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

  val cmds2 = Seq(
    "find 3839442",
    "add 123456 me",
    "add 0 granny",
    "find 0",
    "find 123456",
    "del 0",
    "del 0",
    "find 0"
  )

  val cmds3 = Seq(
    "add 1 A",
    "find 1",
    "add 1 B",
    "find 1",
    "del 1",
    "find 1"
  )

  Main10.process(cmds)

  println("------------")
  Main10.process(cmds2)
  println("------------")
  Main10.process(cmds3)
  println("------------")
}
