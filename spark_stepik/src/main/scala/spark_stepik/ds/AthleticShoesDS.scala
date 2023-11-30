package spark_stepik.ds

import org.apache.spark.sql.functions._
import org.apache.spark.sql.{ Column, DataFrame }
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

  def replaceNull(columnName: String, column: Column): Column =
    coalesce(col(columnName), column).as(columnName)

  def extractColumns(df: DataFrame): DataFrame =
    df.select(
      replaceNull("item_category", lit("n/a")),
      replaceNull("item_name", lit("n/a")),
      replaceNull("item_after_discount", col("item_price")),
      replaceNull("item_rating", lit(0)),
      replaceNull("percentage_solds", lit(-1)),
      replaceNull("buyer_gender", lit("unknown")),
      replaceNull("item_price", lit("n/a")),
      replaceNull("item_shipping", lit("n/a"))
    )

  val processedDF = athleticShoesDF
    .transform(extractColumns)

  import spark.implicits._
  val ds = processedDF.as[Shoes]
  ds.show(10)
}
