package spark_streaming.reviews

import org.apache.spark.sql.{DataFrame, SparkSession}
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._


object StreamReviewAnalyzerAvgRatingOutputModeAppend extends App {

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

  val reviewsStreamDf: DataFrame = spark
    .readStream
    .option("header", "true")
    .schema(schema)
    .csv("spark_streaming/src/main/resources/reviews")


  val ratingByDayStreamDf = reviewsStreamDf
    .withWatermark("date", "1 day")
    .groupBy(window(col("date"), "1 day"))
    .agg(
      avg("rating").alias("avg_rating"))


  ratingByDayStreamDf.writeStream
    .format("console")
    .outputMode("append")
    .start()
    .awaitTermination()


  /*
  +------------------------------------------+----------+
  |window                                    |avg_rating|
  +------------------------------------------+----------+
  |{2023-08-31 00:00:00, 2023-09-01 00:00:00}|8.0       |
  +------------------------------------------+----------+
     */

}