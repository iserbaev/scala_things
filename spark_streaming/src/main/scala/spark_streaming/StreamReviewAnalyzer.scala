package spark_streaming

import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.spark.sql.{ DataFrame, SparkSession }

object StreamReviewAnalyzer extends App {

  val spark = SparkSession
    .builder()
    .appName("ReviewAnalyzer")
    .master("local[*]")
    .getOrCreate()

  spark.conf.set("spark.sql.session.timeZone", "UTC")

  val schema = StructType(
    Array(
      StructField("title", StringType),
      StructField("review", StringType),
      StructField("date", TimestampType),
      StructField("rating", IntegerType),
    )
  )

  val reviewsStreamDf: DataFrame = spark.readStream
    .option("header", "true")
    .schema(schema)
    .csv("src/main/resources/reviews")

  val lowRatingStreamDf = reviewsStreamDf
    .filter(col("rating") < 8)

  lowRatingStreamDf.writeStream
    .format("console")
    .outputMode("append")
    .start()
    .awaitTermination()

  /*
 -------------------------------------------
 Batch: 0
 -------------------------------------------
 +-----------+--------------------+--------------------+------+
 |      title|              review|                date|rating|
 +-----------+--------------------+--------------------+------+
 |Not bad but|Not bad but could...|2023-09-01 07:35:...|     6|
 +-----------+--------------------+--------------------+------+

   */

}
