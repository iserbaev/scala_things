package spark_stepik.ds

import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.functions._
import spark_stepik.SparkCxt
object AthleticShoesDS extends App with SparkCxt {
  case class Shoes(
      item_category: String,
      item_name: String,
      item_after_discount: String,
      item_price: String,
      percentage_solds: Int,
      item_rating: Int,
      item_shipping: String,
      buyer_gender: String
  )

  val athleticShoesDF: DataFrame = spark.read
    .option("header", "true")
    .option("inferSchema", "true")
    .csv("spark_stepik/src/main/resources/2_ds_files/athletic_shoes.csv")

  val processedDF = athleticShoesDF.na
    .drop(Seq("item_name", "item_category"))
    .select(
      col("item_category"),
      col("item_name"),
      coalesce(col("item_after_discount"), col("item_price")).as("item_after_discount"),
      col("item_price"),
      coalesce(col("percentage_solds"), lit(-1)).as("percentage_solds"),
      coalesce(col("item_rating"), lit(0)).as("item_rating"),
      col("item_shipping"),
      coalesce(col("buyer_gender"), lit("unknown")).as("buyer_gender"),
    )
    .na
    .fill("n/a")

  import spark.implicits._
  val ds = processedDF.as[Shoes]
  ds.show(10)
}
