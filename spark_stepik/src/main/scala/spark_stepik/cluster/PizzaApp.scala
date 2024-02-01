package spark_stepik.cluster

import org.apache.log4j.{ Level, Logger }
import org.apache.spark.sql.expressions.Window
import org.apache.spark.sql.functions._
import org.apache.spark.sql.{ DataFrame, SaveMode, SparkSession }

//spark_stepik/src/main/resources/5/pizza_orders.csv
//data/pizza
object PizzaApp {
  Logger.getLogger("org").setLevel(Level.ERROR)

  def main(args: Array[String]): Unit = {

    if (args.length != 2) {
      println("Specify the path to the File and path to save result dataset")
      System.exit(1)
    }

    val csvPath     = args(0)
    val pathToWrite = args(1)

    val spark = SparkSession
      .builder()
      .appName("Pizza App")
      .getOrCreate()

    val pizzaDF = spark.read
      .option("inferSchema", "true")
      .option("header", "true")
      .csv(csvPath)

    val totalOrdersByTypeDF: DataFrame = pizzaDF.groupBy("order_type").agg(count("order_id").as("orders_total"))

    val ordersGroupDF: DataFrame = pizzaDF
      .withColumn("orders_cnt", count("*").over(Window.partitionBy("order_type", "address_id")))
      .select(
        col("order_type").as("otp"),
        col("address_id"),
        col("orders_cnt"),
        row_number().over(Window.partitionBy("order_type").orderBy(col("orders_cnt").desc)).as("rnb")
      )
      .filter(col("rnb") === 1)

    val joinedDF = ordersGroupDF
      .join(totalOrdersByTypeDF, ordersGroupDF.col("otp") === totalOrdersByTypeDF.col("order_type"))
      .select(
        col("order_type"),
        col("address_id"),
        col("orders_cnt"),
        col("orders_total"),
      )

    joinedDF.write
      .mode(SaveMode.Overwrite)
      .save(pathToWrite)
  }
}
