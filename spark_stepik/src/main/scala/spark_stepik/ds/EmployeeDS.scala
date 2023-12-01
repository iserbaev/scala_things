package spark_stepik.ds

import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.functions._
import spark_stepik.SparkCxt

object EmployeeDS extends App with SparkCxt {
  override val appName: String = "EmployeeDS"

  val employeeDF: DataFrame = spark.read
    .option("header", "true")
    .csv("spark_stepik/src/main/resources/2_ds_files/employee.csv")

  employeeDF
    .select("*")
    .where(col("birthday").isNull)
    .show()

  employeeDF
    .select("*")
    .orderBy(col("date_of_birth").desc_nulls_first)
    .show()

  employeeDF.na
    .drop()
    .show()

  employeeDF.na
    .fill("n/a", List("birthday", "date_of_birth"))
    .show()

  employeeDF.na
    .fill(
      Map(
        "birthday"      -> "n/a",
        "date_of_birth" -> "Unknown",
      )
    )
    .show()

  employeeDF
    .select(col("name"), coalesce(col("birthday"), col("date_of_birth")))
    .show()

  employeeDF
    .select(col("name"), coalesce(col("birthday"), col("date_of_birth"), lit("n/a")))
    .show()

  employeeDF
    .selectExpr(
      "name",
      "birthday",
      "date_of_birth",
      "ifnull(birthday, date_of_birth) as ifnull",
      "nvl(birthday, date_of_birth) as nvl",
      "nvl2(birthday, date_of_birth, 'Unknown') as nvl2"
    )
    .show()
}
