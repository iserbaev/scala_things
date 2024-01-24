package spark_stepik.cluster

import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.{SaveMode, SparkSession}

//spark_stepik/src/main/resources/5/pizza_orders.csv
//data/pizza
object PizzaApp {
  Logger.getLogger("org").setLevel(Level.ERROR)

  def main(args: Array[String]): Unit = {

    if (args.length != 2) {
      println("Specify the path to the File and path to save result dataset")
      System.exit(1)
    }

    val csvPath = args(0)
    val pathToWrite = args(1)

    val spark = SparkSession.builder()
      .appName("Pizza App")
      .getOrCreate()


    val pizzaDF = spark.read
      .option("inferSchema", "true")
      .option("header", "true")
      .csv(csvPath)

    pizzaDF.printSchema()

    pizzaDF.show()

    pizzaDF.write
      .mode(SaveMode.Overwrite)
      .save(pathToWrite)
  }

}