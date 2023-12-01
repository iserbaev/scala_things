package spark_stepik.ds

import org.apache.spark.sql.catalyst.encoders.ExpressionEncoder
import org.apache.spark.sql.functions._
import org.apache.spark.sql.{ DataFrame, Dataset }
import spark_stepik.SparkCxt

object ChannelsDS extends App with SparkCxt {
  val channelsDF: DataFrame = spark.read
    .option("header", "true")
    .option("inferSchema", "true")
    .csv("spark_stepik/src/main/resources/2_ds_files/channel.csv")

  case class Channel(
      channel_name: String,
      city: String,
      country: String,
      created: String,
  )

  import spark.implicits._

  val channelsDS = channelsDF.as[Channel]
  channelsDF.show(10)

  /* -UpperCase DF vs DS- */
  val cityNamesDF = channelsDF
    .select(
      upper(col("city")).as("city")
    )

  val cityNamesDS: Dataset[String] =
    channelsDS.map(channel => channel.city.toUpperCase)

  cityNamesDF.show(10)
  cityNamesDS.show(10)

  /* -Dates DF vs DS- */
  val withChannelAgeDF = channelsDF
    .withColumn("today", to_date(lit("04/19/2022"), "MM/dd/yyyy"))
    .withColumn("actual_date", to_date(col("created"), "yyyy MMM dd"))
    .withColumn(
      "channel_age",
      datediff(col("today"), col("actual_date"))
    )

  case class Age(
      channel_name: String,
      age: String,
  )
  implicit val encoder: ExpressionEncoder[Age] = ExpressionEncoder[Age]()

  import java.text.SimpleDateFormat
  def toDate(date: String, dateFormat: String): Long = {
    val format = new SimpleDateFormat(dateFormat)
    format.parse(date).getTime // milliseconds
  }
  def countChannelAge(channel: Channel): Age = {
    val age = (toDate("04/19/2022", "MM/dd/yyyy") - toDate(channel.created, "yyyy MMM dd")) / (1000 * 60 * 60 * 24)
    Age(channel.channel_name, age.toString)
  }
  val ageDS = channelsDS.map(channel => countChannelAge(channel))

  val joinedDS: Dataset[(Channel, Age)] = channelsDS
    .joinWith(ageDS, channelsDS.col("channel_name") === ageDS.col("channel_name"))
  val joinedDF: DataFrame = channelsDS
    .join(ageDS, channelsDS.col("channel_name") === ageDS.col("channel_name"))

  withChannelAgeDF.printSchema()
  joinedDS.printSchema()
  joinedDF.printSchema()
}
