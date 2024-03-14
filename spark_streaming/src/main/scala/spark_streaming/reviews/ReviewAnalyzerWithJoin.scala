package spark_streaming.reviews

import org.apache.spark.sql.functions._
import org.apache.spark.sql.streaming.Trigger
import org.apache.spark.sql.types.StructType
import org.apache.spark.sql.{DataFrame, SparkSession}

object ReviewAnalyzerWithJoin extends App {

  val spark = SparkSession
    .builder()
    .appName("ReviewAnalyzer")
    .master("local[*]")
    .getOrCreate()

  spark.conf.set("spark.sql.session.timeZone", "UTC")

  val reviewsDf = spark.read
    .option("header", true)
    .option("inferSchema", true)
    .csv("spark_streaming/src/main/resources/reviews/reviews.csv")

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

  def read(file: String): DataFrame =
    spark.read
      .option("header", true)
      .option("inferSchema", true)
      .csv(file)

  val employeeReviewsDf = read("spark_streaming/src/main/resources/employee_reviews/employee_reviews.csv")
  val departmentsDf     = read("spark_streaming/src/main/resources/departments/departments.csv")

  val joinedDf = employeeReviewsDf
    .join(departmentsDf, employeeReviewsDf.col("Department") === departmentsDf.col("dept_code"))
    .select("Title", "Department", "dept_name")

  joinedDf
    .show()

  val emplRreviewsSchema    = employeeReviewsDf.schema
  val departmentSchema = departmentsDf.schema

  def readStream(directory: String, schema: StructType): DataFrame =
    spark.readStream
      .option("header", "true")
      .schema(schema)
      .csv(directory)

  val reviewsStreamDf = readStream("spark_streaming/src/main/resources/employee_reviews", emplRreviewsSchema)

  val departmentsStreamDf = readStream("spark_streaming/src/main/resources/departments", departmentSchema)

  // Когда дело касается потокового датафрейма, то код для join будет тот же самый.
  // Причем заметьте, что объединять можно как потоковый датафрейм с обычным, так и потоковый датафрейм с потоковым:

  val joinedStreamDf1 = reviewsStreamDf
    .join(
      departmentsDf, // объединяем с обычным датафреймом
      reviewsStreamDf.col("Department") === departmentsDf.col("dept_code")
    )
    .select("Title", "Department", "dept_name")

  val joinQuery1 = joinedStreamDf1.writeStream
    .format("console")
    .outputMode("append")
    .trigger(Trigger.AvailableNow())
    .start()

  joinQuery1.awaitTermination()

  // Не каждый джойн можно выполнить, объединяя потоковой и обычный датафрейм. При выполнении следующих джойнов будет AnalysisException:

    // right_outer join
    // full_outer join
    // right_semi join
    // В этом вы можете убедиться сами, попробовав выполнить код:

//  val joinedStreamDf2 = reviewsStreamDf
//    .join(
//      departmentsDf,
//      reviewsStreamDf.col("Department") === departmentsDf.col("dept_code"),
//      "full_outer")  // указали недопустимый тип джойна
//    .select(
//      "Title",
//      "Department",
//      "dept_name")
//
//
//  val joinQuery2 = joinedStreamDf2
//    .writeStream
//    .format("console")
//    .outputMode("append")
//    .start()
//
//  joinQuery2.awaitTermination()

  /*
  AnalysisException: FullOuter joins with streaming DataFrames/Datasets
  on the left and a static DataFrame/Dataset on the right is not supported
  */

  // Причина заключается в том, что для выполнения обозначенных джойнов потребуется хранить абсолютно все данные,
  // связанные с потоковым датафреймом. А это просто невозможно.



  // Когда же идет объединение двух потоковых датафреймов, то для получения результатов доступен только outputMode("append"):

  val joinedTwoStreamDf3 = reviewsStreamDf
    .join(
      departmentsStreamDf,
      reviewsStreamDf.col("Department") === departmentsStreamDf.col("dept_code"))
    .select("Title", "Department", "dept_name")

  val joinTwoStreamsQuery = joinedTwoStreamDf3
    .writeStream
    .format("console")
    .outputMode("append")
    .trigger(Trigger.AvailableNow())
    .start()

  joinTwoStreamsQuery.awaitTermination()
  // При этом следующие типы джойнов доступны только при указании водяных знаков:

  // right_outer
  // left_outer
  // full_outer
}
