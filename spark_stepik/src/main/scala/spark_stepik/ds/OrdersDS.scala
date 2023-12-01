package spark_stepik.ds

import org.apache.spark.sql.types.StructType
import org.apache.spark.sql.{ Dataset, Encoder, Encoders, Row }
import org.apache.spark.sql.functions._
import spark_stepik.SparkCxt

object OrdersDS extends App with SparkCxt {
  case class Order1(
      orderId: Int,
      customerId: Int,
      product: String,
      quantity: Int,
      priceEach: Double
  )

  val orders1Data: Seq[Row] = Seq(
    Row(1, 2, "USB-C Charging Cable", 3, 11.29),
    Row(2, 3, "Google Phone", 1, 600.33),
    Row(2, 3, "Wired Headphones", 2, 11.90),
    Row(3, 2, "AA Batteries (4-pack)", 4, 3.85),
    Row(4, 3, "Bose SoundSport Headphones", 1, 99.90),
    Row(4, 3, "20in Monitor", 1, 109.99)
  )

  val orders1Schema: StructType = Encoders.product[Order1].schema

  import spark.implicits._

  val orders1DS = spark
    .createDataFrame(
      spark.sparkContext.parallelize(orders1Data),
      orders1Schema
    )
    .as[Order1]

  orders1DS.show(10)

  /* Найти:
    - сколько всего предметов было куплено (orderQuantity)
    - общую сумму всех заказов (price)
   */
  def getTotalStats(orders: Dataset[Order1]): (Double, Int) = {
    val stats: (Double, Int) = orders
      .map(order => (order.priceEach * order.quantity, order.quantity))
      .reduce((a, b) => (a._1 + b._1, a._2 + b._2))

    (stats._1, stats._2)
  }

  val (price, orderQuantity) = getTotalStats(orders1DS) // результат: (883.29, 12)
  println(price)
  println(orderQuantity)

  /* Найти общую сумму заказов для каждого покупателя. При решении будем стремиться к тому,
     чтобы типом возвращаемого значения оставался Dataset (не менялся на DataFrame).
   */
  case class CustomerInfo(
      customerId: Int,
      priceTotal: Double
  )

  val infoDS: Dataset[CustomerInfo] = orders1DS
    .groupByKey(_.customerId)
    .mapGroups { (id, orders) =>
      val priceTotal = orders.map(order => order.priceEach * order.quantity).sum

      CustomerInfo(
        id,
        priceTotal
      )
    }

  infoDS.show()

  /* Возьмем два датасета: customersDS и ordersDS; На основании данных, имеющихся в этих датасетах, составим датасет salesDS. */
  case class Sales(
      customer: String,
      product: String,
      price: Double,
  )

  case class Customer(id: Int, email: String, orders: Seq[Int])

  case class Order(orderId: Int, product: String, quantity: Int, priceEach: Double)

  val customerData: Seq[Row] = Seq(
    Row(1, "Bob@example.com", Seq()),
    Row(2, "alice@example.com", Seq(1, 3)),
    Row(3, "Sam@example.com", Seq(2, 4))
  )

  val ordersData: Seq[Row] = Seq(
    Row(1, "USB-C Charging Cable", 3, 11.29),
    Row(2, "Google Phone", 1, 600.33),
    Row(2, "Wired Headphones", 2, 11.90),
    Row(3, "AA Batteries (4-pack)", 4, 3.85),
    Row(4, "Bose SoundSport Headphones", 1, 99.90),
    Row(4, "20in Monitor", 1, 109.99)
  )

  def toDS[T <: Product: Encoder](data: Seq[Row], schema: StructType): Dataset[T] =
    spark
      .createDataFrame(
        spark.sparkContext.parallelize(data),
        schema
      )
      .as[T]

  val ordersSchema: StructType   = Encoders.product[Order].schema
  val customerSchema: StructType = Encoders.product[Customer].schema

  val customersDS = toDS[Customer](customerData, customerSchema)
  val ordersDS    = toDS[Order](ordersData, ordersSchema)

  val joinedDS: Dataset[(Customer, Order)] = customersDS
    .joinWith(
      ordersDS,
      array_contains(customersDS.col("orders"), ordersDS.col("orderId")),
      "outer"
    )

  joinedDS.show(false)

  val salesDS: Dataset[Sales] = joinedDS
    .filter(record => record._1.orders.nonEmpty)
    .map(record =>
      Sales(
        record._1.email.toLowerCase(),
        record._2.product,
        record._2.quantity * record._2.priceEach
      )
    )

  salesDS.show()

  val salesWithEmptyOrdersDS: Dataset[Sales] = joinedDS
    .map(record => record._1.orders.isEmpty match {
      case false => Sales(
        record._1.email.toLowerCase(),
        record._2.product,
        record._2.quantity * record._2.priceEach)
      case _ => Sales(
        record._1.email.toLowerCase(),
        "-",
        0.0)
    })

  salesWithEmptyOrdersDS.show()
}
