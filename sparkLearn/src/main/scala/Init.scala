object Init extends App {
  import org.apache.spark.{SparkConf, SparkContext}

  val conf = new SparkConf().setMaster("local").setAppName("My App")
  val sc = new SparkContext(conf)

  val res = sc.parallelize(1 to 100000).count()
  println(res)

  sc.stop()

  def countWords(inputPath: String, outputPath: String) = {
    val input = sc.textFile(inputPath)
    val words = input.flatMap(line => line.split(" "))
    val counts = words.map(word => (word, 1)).reduceByKey{case (x, y) => x + y}
    counts.saveAsTextFile(outputPath)
  }

}
