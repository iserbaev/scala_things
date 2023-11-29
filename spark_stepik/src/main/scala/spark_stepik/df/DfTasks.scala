package spark_stepik.df

import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import spark_stepik.SparkCxt

object DfTasks extends App with SparkCxt {
  override val appName: String = "DfTasks"

  val mallCustomersSchema: StructType = StructType(
    Seq(
      StructField("CustomerID", IntegerType),
      StructField("Gender", StringType),
      StructField("Age", ShortType),
      StructField("Annual Income (k$)", IntegerType),
      StructField("Spending Score (1-100)", IntegerType)
    )
  )

  val mallCustomersRawDF: DataFrame = spark.read
    .format("csv")
    .schema(mallCustomersSchema)
    .option("header", "true")
    .option("path", "spark_stepik/src/main/resources/1_df_files/mall_customers.csv")
    .load()

  val mallCustomersAgeFixedDF: DataFrame =
    mallCustomersRawDF.withColumn("Age", col("Age") + 2)

  val incomeDF: DataFrame = mallCustomersAgeFixedDF
    .filter("30 <= Age")
    .filter("Age <= 35")
    .groupBy("Gender", "Age")
    .agg(round(avg("Annual Income (k$)"), 1).as("Average Annual Income (k$)"))
    .sort("Gender", "Age")

  val isMale: Column = col("Gender") === "Male"
  val incomeWithGenderCodeDF: DataFrame = incomeDF
    .withColumn("gender_code", when(isMale, 1).otherwise(0))

  incomeWithGenderCodeDF.write
    .mode(SaveMode.Overwrite)
    .save("spark_stepik/src/main/resources/data/customers")
}
