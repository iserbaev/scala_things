package spark_stepik.df

import org.apache.spark.sql.SaveMode
import org.apache.spark.sql.types._
import spark_stepik.SparkCxt

object DF2 extends App with SparkCxt {
  override val appName: String = "DF2"

  val moviesSchema = StructType(
    Seq(
      StructField("id", IntegerType),
      StructField("show_id", StringType),
      StructField("show_type", StringType),
      StructField("title", StringType),
      StructField("director", StringType),
      StructField("cast", StringType, nullable = true),
      StructField("country", StringType),
      StructField("date_added", StringType),
      StructField("release_year", IntegerType),
      StructField("rating", StringType),
      StructField("duration", IntegerType),
      StructField("listed_in", StringType),
      StructField("description", StringType),
      StructField("year_added", IntegerType),
      StructField("month_added", DoubleType),
      StructField("season_count", IntegerType)
    )
  )

  val moviesOnNetflixDF = spark.read
    .schema(moviesSchema)
    .format("csv")
    .option("path", "spark_stepik/src/main/resources/1_df_files/movies_on_netflix.csv")
    .load()
  moviesOnNetflixDF.show(2)
  moviesOnNetflixDF.printSchema()

  moviesOnNetflixDF.write
    .mode(SaveMode.Overwrite)
    .save("spark_stepik/src/main/resources/data")

}
