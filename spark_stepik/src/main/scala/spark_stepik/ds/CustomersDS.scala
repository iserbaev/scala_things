package spark_stepik.ds

import org.apache.spark.sql.DataFrame
import spark_stepik.SparkCxt

object CustomersDS extends App with SparkCxt {
  override val appName: String = "CustomersDS"

  case class Customer(
      name: String,
      surname: String,
      age: Int,
      occupation: String,
      customer_rating: Double
  )

  val customersDF: DataFrame = spark.read
    .option("header", "true")
    .option("inferSchema", "true")
    .option("sep", ",")
    .csv("spark_stepik/src/main/resources/2_ds_files/customers.csv")

  import spark.implicits._

  val customersDS = customersDF.as[Customer]

  customersDS.show(10)
}
