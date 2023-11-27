package spark_stepik.df

import org.apache.spark.rdd.RDD
import org.apache.spark.sql.types._
import org.apache.spark.sql.{ Row, SparkSession }

object DF1 extends App {

  val spark = SparkSession
    .builder()
    .appName("DF1")
    .master("local")
    .getOrCreate()
  val sc = spark.sparkContext

  val data: Seq[Row] = Seq(
    Row("s9FH4rDMvds", "2020-08-11T22:21:49Z", "UCGfBwrCoi9ZJjKiUK8MmJNw", "2020-08-12T00:00:00Z"),
    Row("kZxn-0uoqV8", "2020-08-11T14:00:21Z", "UCGFNp4Pialo9wjT9Bo8wECA", "2020-08-12T00:00:00Z"),
    Row("QHpU9xLX3nU", "2020-08-10T16:32:12Z", "UCAuvouPCYSOufWtv8qbe6wA", "2020-08-12T00:00:00Z")
  )

  val schema: Array[StructField] = Array(
    StructField("videoId", StringType, false),
    StructField("publishedAt", StringType, false),
    StructField("channelId", StringType, false),
    StructField("trendingDate", StringType, false),
  )
  val rdd: RDD[Row] = sc.parallelize(data)

  val df = spark.createDataFrame(rdd, StructType(schema))

  df.show()
  df.printSchema()

  val restaurantSchema = StructType(
    Seq(
      StructField("average_cost_for_two", LongType),
      StructField("cuisines", StringType),
      StructField("deeplink", StringType),
      StructField("has_online_delivery", IntegerType),
      StructField("is_delivering_now", IntegerType),
      StructField("menu_url", StringType),
      StructField("name", StringType),
      StructField("opened", StringType),
      StructField("photos_url", StringType),
      StructField("url", StringType),
      StructField(
        "user_rating",
        StructType(
          Seq(
            StructField("aggregate_rating", StringType),
            StructField("rating_color", StringType),
            StructField("rating_text", StringType),
            StructField("votes", StringType)
          )
        )
      )
    )
  )

  val restaurantDF = spark.read
    .format("json")                // проинструктировали spark reader прочитать файл в формате json
    .option("inferSchema", "true") // предоставляем спарку самому составить схему данных
    .load("spark_stepik/src/main/resources/1_df_files/restaurant.json") // указываем путь к файлу
  restaurantDF.show(2)
  restaurantDF.printSchema()

  val restaurantDF2 = spark.read
    .schema(restaurantSchema)
    .json("spark_stepik/src/main/resources/1_df_files/restaurant.json")
  restaurantDF2.show(2)
  restaurantDF2.printSchema()

  val irisDF = spark.read
    .format("json")
    .option("inferSchema", "true")
    .load("spark_stepik/src/main/resources/1_df_files/iris.json")
  irisDF.show(2)
  irisDF.printSchema()

  irisDF.take(3).foreach(println)

  val restaurantExSchema = StructType(
    Seq(
      StructField("has_online_delivery", IntegerType),
      StructField("url", StringType),
      StructField(
        "user_rating",
        StructType(
          Seq(
            StructField("aggregate_rating", StringType),
            StructField("rating_color", StringType),
            StructField("rating_text", StringType),
            StructField("votes", StringType)
          )
        )
      ),
      StructField("name", StringType),
      StructField("cuisines", StringType),
      StructField("is_delivering_now", IntegerType),
      StructField("deeplink", StringType),
      StructField("menu_url", StringType),
      StructField("average_cost_for_two", IntegerType)
    )
  )
  val restaurantExDF = spark.read
    .schema(restaurantExSchema)
    .json("spark_stepik/src/main/resources/1_df_files/restaurant_ex.json")
  restaurantExDF.show(2)
  restaurantExDF.printSchema()
}
