package spark_streaming.reviews

import org.apache.spark.sql.{DataFrame, SparkSession}
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._


object StreamReviewAnalyzerAvgRatingOutputModeComplete extends App {

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
    .groupBy(date_format(
      col("date"), "MM-dd-yyyy").as("date"))
    .agg(
      avg("rating").alias("avg_rating"))


  ratingByDayStreamDf.writeStream
    .format("console")
    .outputMode("complete")
    .start()
    .awaitTermination()


  /*
    +----------+----------+
    |      date|avg_rating|
    +----------+----------+
    |08-31-2023|       8.0|
    |09-01-2023|       8.5|
    |09-02-2023|      10.0|
    +----------+----------+
     */

}