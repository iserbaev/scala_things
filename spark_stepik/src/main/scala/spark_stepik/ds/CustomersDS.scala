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

  val customersWithNullsDF: DataFrame = spark.read
    .option("header", "true")
    .option("inferSchema", "true")
    .csv("spark_stepik/src/main/resources/2_ds_files/customers_with_nulls.csv")

  case class CustomerWithNulls(
      name: String,
      age: Option[Int],
      customer_rating: Option[Double])

  val customersWithNullsDS = customersWithNullsDF.as[CustomerWithNulls]

  customersWithNullsDS
    .filter(customer => customer.customer_rating.getOrElse(0.0) > 4.0)
    .show()
}
