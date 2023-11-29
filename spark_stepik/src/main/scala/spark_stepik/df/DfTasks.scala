package spark_stepik.df

import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import spark_stepik.SparkCxt

object DfTasks extends App with SparkCxt {
  override val appName: String = "DfTasks"

  def withProperAge(df: DataFrame): DataFrame =
    df.withColumn("Age", col("Age").plus(2))

  def withGenderCode(df: DataFrame): DataFrame = {
    val isFemale = col("Gender") === "Female"
    val isMale   = col("Gender") === "Male"

    df.withColumn(
      "gender_code",
      when(isMale, 1)
        .when(isFemale, 0)
        .otherwise(-1)
    )
  }

  def extractCustomerGroups(df: DataFrame): DataFrame = {
    val columns      = Seq(col("Gender"), col("Age"))
    val hasProperAge = col("Age").between(30, 35)

    df
      .filter(hasProperAge)
      .groupBy(columns: _*)
      .agg(round(avg("Annual Income (k$)"), 1).as("Avg_Income"))
      .orderBy(columns: _*)
  }

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

  val incomeWithGenderCodeDF = mallCustomersRawDF
    .transform(withProperAge)
    .transform(extractCustomerGroups)
    .transform(withGenderCode)

  incomeWithGenderCodeDF.write
    .mode(SaveMode.Overwrite)
    .save("spark_stepik/src/main/resources/data/customers")
}
