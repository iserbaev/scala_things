package spark_streaming.iot

import org.apache.spark.sql.functions._
import org.apache.spark.sql.streaming.Trigger
import org.apache.spark.sql.types._
import org.apache.spark.sql.{Column, DataFrame, SparkSession}

object StreamIotDataAnalyzer extends App {

  val spark = SparkSession
    .builder()
    .appName("IotDataAnalyzer")
    .master("local[*]")
    .getOrCreate()

  spark.conf.set("spark.sql.session.timeZone", "UTC")

  val schema = StructType(
    Array(
      StructField("ts", FloatType),
      StructField("device", StringType),
      StructField("co", StringType),
      StructField("humidity", StringType),
      StructField("light", StringType),
      StructField("lpg", StringType),
      StructField("motion", StringType),
      StructField("smoke", StringType),
      StructField("temp", DoubleType)
    )
  )

  val iotDatasStreamDf: DataFrame = spark.readStream
    .option("header", "true")
    .schema(schema)
    .csv("spark_streaming/src/main/resources/iot_data")

  def toCelsius(temp: Column): Column = {
    (temp - 32) / 1.8
  }

  def extractData(df: DataFrame) = {
    df.select(
      col("device"),
      toCelsius(col("temp")).as("temp_celsius"),
      from_unixtime(col("ts")).cast(TimestampType).as("timestamp"))
  }

  def getAvgTemp(groupByCols: Seq[Column])(df: DataFrame) = {
    df
      .withWatermark(
        "timestamp",
        "2 minutes")
      .groupBy(
        groupByCols: _*)
      .agg(
        avg(col("temp_celsius")))
  }


  val slidingWindow = window(
    col("timestamp"),
    "2 minutes",
    "1 minutes")

  val lowRatingStreamDf = iotDatasStreamDf
    .transform(extractData)
    .transform(
      getAvgTemp(Seq(
        slidingWindow,
        col("device"))))

  lowRatingStreamDf.writeStream
    .format("console")
    .outputMode("append")
    .trigger(Trigger.AvailableNow())
    .start()
    .awaitTermination()
}
