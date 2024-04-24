package spark_streaming
package newsaggregator

import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.functions._
import org.apache.spark.sql.streaming.{ OutputMode, Trigger }
import org.apache.spark.sql.types.{ StringType, StructField, StructType, TimestampType }

object NewsAggregatorAnalyzer extends App with Context {
  override val appName: String = "News Aggregator Analyzer"

  import spark.implicits._

  val newsCategorySchema = StructType(
    Array(
      StructField("category", StringType),
      StructField("category_name", StringType)
    )
  )

  val newsAggregatorSchema = StructType(
    Array(
      StructField("id", StringType),
      StructField("title", StringType),
      StructField("url", StringType),
      StructField("publisher", StringType),
      StructField("category", StringType),
      StructField("story", StringType),
      StructField("timestamp", StringType)
    )
  )

  val newsCategoryDf: DataFrame = spark.read
    .option("header", "true")
    .schema(newsCategorySchema)
    .csv("spark_streaming/src/main/resources/newscategory")

  println(newsCategoryDf.count())

  val newsAggregatorStreamDf: DataFrame = spark.readStream
    .option("header", "true")
    .schema(newsAggregatorSchema)
    .csv("spark_streaming/src/main/resources/newsaggregator")

  val domainRegex = "^(?:http[s]?://)?(?:[^@\\n]+@)?(?:www.)?([^:/\\n?]+)"
  def extractData(df: DataFrame): DataFrame =
    df.select(
      col("category").as("category"),
      regexp_extract(col("url"), domainRegex, 1).as("site"),
      from_unixtime(col("timestamp") / 1000).cast(TimestampType).as("ts")
    )

  def groupByDayCategorySite(df: DataFrame): DataFrame =
    df.groupBy(dailyTumblingWindow.as("day"), col("category"), col("site"))
      .count()
      .as("na")

  def joinWithNewsCategory(df: DataFrame) =
    df.join(
      broadcast(newsCategoryDf).as("nc"),
      $"na.category" === $"nc.category"
    )

  val dailyTumblingWindow = window(col("ts"), "1 day")

  val dailyNewsByCategoryCountStreamDf: DataFrame =
    newsAggregatorStreamDf
      .transform(extractData)
      .transform(groupByDayCategorySite)
      .transform(joinWithNewsCategory)
      .select(
        $"count",
        $"day",
        $"site",
        $"category_name"
      )
      .orderBy($"count".desc)

  dailyNewsByCategoryCountStreamDf.writeStream
    .format("console")
    .outputMode(OutputMode.Complete())
    .option("truncate", "false")
    .trigger(Trigger.AvailableNow())
    .start()
    .awaitTermination()

}
