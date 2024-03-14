package spark_streaming.reviews

import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions._


object BatchReviewAnalyzer extends App {

  val spark = SparkSession.builder()
    .appName("ReviewAnalyzer")
    .master("local[*]")
    .getOrCreate()

  spark.conf.set("spark.sql.session.timeZone", "UTC")

  val reviewsDf = spark.read
    .option("header", true)
    .option("inferSchema", true)
    .csv("src/main/resources/reviews/reviews.csv")


  val lowRatingDf = reviewsDf
    .filter(col("rating") < 8)
    .orderBy(desc("rating"))

  lowRatingDf.show()


  /*
+-----------+--------------------+--------------------+------+
|      title|              review|                date|rating|
+-----------+--------------------+--------------------+------+
|Not bad but|Not bad but could...|2023-09-01 07:35:...|     6|
+-----------+--------------------+--------------------+------+
   */

}