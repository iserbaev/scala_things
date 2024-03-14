package spark_streaming.reviews

import org.apache.spark.sql.{DataFrame, SparkSession}
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._


object BatchReviewAnalyzerAvgRating extends App {

  val spark = SparkSession.builder()
    .appName("ReviewAnalyzer")
    .master("local[*]")
    .getOrCreate()

  spark.conf.set("spark.sql.session.timeZone", "UTC")

  val schema = StructType(Array(
    StructField("title", StringType),
    StructField("review", StringType),
    StructField("date", TimestampType),
    StructField("rating", IntegerType),
  ))


  val reviewsDf = spark.read
    .option("header", true)
    .schema(schema)
    .csv("src/main/resources/reviews/reviews.csv")


  val ratingByDayDf = reviewsDf
    .groupBy(date_format(
      col("date"), "MM-dd-yyyy").as("date"))
    .agg(avg("rating").alias("avg_rating"))
    .orderBy(desc("avg_rating"))


  ratingByDayDf.show()


  /*
+----------+----------+
|      date|avg_rating|
+----------+----------+
|09-02-2023|      10.0|
|09-01-2023|       8.5|
|08-31-2023|       8.0|
+----------+----------+
   */

}