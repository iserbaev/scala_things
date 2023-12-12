package spark_stepik.rdd

import org.apache.spark.rdd.RDD
import spark_stepik.SparkCxt

object StoresRDD extends App with SparkCxt {
  case class Store(
      state: String,
      location: String,
      address: String,
      latitude: Double,
      longitude: Double
  )

  def readStores(filename: String) =
    scala.io.Source.fromFile(filename)
      .getLines()
      //удаляем первую строчку, тк в ней содержатся названия колонок
      .drop(1)
      // данные в колонках разделяются запятой
      .map(line => line.split(","))
      // построчно считываем данные в case класс
      .map(values => Store(
        values(0),
        values(1),
        values(2),
        values(3).toDouble,
        values(4).toDouble)
      ).toList

  //1. обычное считывание данных из файла, которое возможно сделать средствами import scala.io.Source
  val storesRDD = sc.parallelize(readStores("spark_stepik/src/main/resources/3_rdd_part1/chipotle_stores.csv"))

  storesRDD.foreach(println)

  //2. использование метода  .textFile
  val storesRDD2 = sc.textFile("spark_stepik/src/main/resources/3_rdd_part1/chipotle_stores.csv")
    .map(line => line.split(","))
    .filter(values => values(0) == "Alabama")
    .map(values => Store(
      values(0),
      values(1),
      values(2),
      values(3).toDouble,
      values(4).toDouble))

  storesRDD2.foreach(println)

  //3. Из DF в RDD
  //Самый легкий, но в то же время самый ресурсозатратный метод из-за необходимости проведения конвертации.

  //3.1 Напрямую из DF в RDD[Row]
  val storesDF = spark.read
    .option("header", "true")
    .option("inferSchema", "true")
    .csv("spark_stepik/src/main/resources/3_rdd_part1/chipotle_stores.csv")


  val storesRDD3 = storesDF.rdd

  storesRDD3.foreach(println) // [Alabama,Auburn,AL 36832 US,32.606812966051244,-85.48732833164195]

  //3.2 DF -> DS ->  RDD[Store]
  import spark.implicits._

  val storesDS = storesDF.as[Store]
  val storesRDD4 = storesDS.rdd

  storesRDD4.foreach(println) // Store(Alabama,Auburn,AL 36832 US,32.606812966051244,-85.48732833164195)

  val locationNamesRDD: RDD[String] = storesRDD.map(_.location).distinct()
  locationNamesRDD.foreach(println)

  implicit val storeOrdering: Ordering[Store] =
    Ordering.fromLessThan[Store]((sa: Store, sb: Store) => sa.location.length < sb.location.length)
  val longestLocationName = storesRDD.max().location
  println(s"location = $longestLocationName") // Birmingham

  val locationRDD = storesRDD.filter(_.location == longestLocationName)
  locationRDD.foreach(println)

  val groupedStoresRDD = storesRDD.groupBy(_.location)
  groupedStoresRDD.foreach(println)
}
