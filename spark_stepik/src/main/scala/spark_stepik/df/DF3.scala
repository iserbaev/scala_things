package spark_stepik.df

import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import spark_stepik.SparkCxt

object DF3 extends App with SparkCxt {
  override val appName: String = "DF3"

  val bikeSharingSchema = StructType(
    Seq(
      StructField("Date", StringType),
      StructField("RENTED_BIKE_COUNT", IntegerType),
      StructField("Hour", ByteType),
      StructField("TEMPERATURE", DoubleType),
      StructField("HUMIDITY", DoubleType),
      StructField("WIND_SPEED", DoubleType),
      StructField("Visibility", DoubleType),
      StructField("DEW_POINT_TEMPERATURE", DoubleType),
      StructField("SOLAR_RADIATION", DoubleType),
      StructField("RAINFALL", IntegerType),
      StructField("Snowfall", IntegerType),
      StructField("SEASONS", StringType),
      StructField("HOLIDAY", StringType),
      StructField("FUNCTIONING_DAY", StringType)
    )
  )

  val bikeSharingDF = spark.read
    .format("csv")
    .schema(bikeSharingSchema)
    .option("header", "true")
    .option("path", "spark_stepik/src/main/resources/1_df_files/bike_sharing.csv")
    .load()

  bikeSharingDF.select("Hour", "TEMPERATURE", "HUMIDITY", "WIND_SPEED").show(3)

  bikeSharingDF
    .select(
      "Date",
      "RENTED_BIKE_COUNT",
      "Hour"
    )
    .printSchema()

  val count = bikeSharingDF
    .filter(
      "RENTED_BIKE_COUNT == 254"
    )
    .filter("TEMPERATURE > 0")
    .count()
  println(count)

  val isWorkday =
    col("HOLIDAY") === "Holiday" &&
      col("FUNCTIONING_DAY") === "No"

  val resultDF = bikeSharingDF
    .select("HOLIDAY", "FUNCTIONING_DAY")
    .distinct()
    .withColumn("is_workday", when(isWorkday, 0).otherwise(1))

  resultDF.show(3)

  val minMaxTempByDateDF = bikeSharingDF
    .groupBy("Date")
    .agg(
      min("TEMPERATURE").as("min_temp"),
      max("TEMPERATURE").as("max_temp")
    )
    .orderBy("Date")

  minMaxTempByDateDF.show(5)
}
