//package spark_stepik.rdd
//
//import org.apache.spark.rdd.RDD
//import org.apache.spark.sql.functions._
//import org.apache.spark.sql.{ DataFrame, Dataset }
//import spark_stepik.SparkCxt
//
//import java.time.LocalDate
//import scala.collection.MapView
//
//object AvocadoRDD extends App with SparkCxt {
//
//  final case class Avocado(id: Int, date: String, avgPrice: Double, volume: Double, year: String, region: String) {
//    def localDate: LocalDate = LocalDate.parse(date)
//  }
//
//  val avocadoDF: DataFrame = spark.read
//    .option("header", "true")
//    .option("inferSchema", "true")
//    .csv("spark_stepik/src/main/resources/3_rdd_part1/avocado.csv")
//
//  def extractColumns(df: DataFrame): DataFrame =
//    df.select(
//      col("id"),
//      col("Date").as("date"),
//      col("AveragePrice").as("avgPrice"),
//      col("TotalVolume").as("volume"),
//      col("year"),
//      col("region")
//    ).where(col("Date").isNotNull)
//
//  import spark.implicits._
//  val avocadoDS: Dataset[Avocado] = avocadoDF.transform(extractColumns).as[Avocado]
//  val avocadoRDD: RDD[Avocado]    = avocadoDS.rdd
//
//  avocadoRDD.foreach(println)
//
//  // подсчитайте количество уникальных регионов (region), для которых представлена статистика
//  // выберите и отобразите на экране все записи о продажах авокадо, сделанные после 2018-02-11
//  // найдите месяц, который чаще всего представлен в статистике
//  // найдите максимальное и минимальное значение avgPrice
//  // отобразите средний объем продаж (volume) для каждого региона (region)
//
//  case class AggregationResult(
//      regionsWithVolumeCountAndSum: Map[String, (Int, Double)],
//      maxAvgPrice: Double,
//      minAvgPrice: Double,
//      monthCounts: Map[Int, Int]
//  ) {
//    def avgVolumesByRegions: MapView[String, Double] =
//      regionsWithVolumeCountAndSum.view.mapValues { case (i, d) => d / i }
//
//    def maxFrequentMonth: Option[Int] = monthCounts.maxByOption(_._2).map(_._1)
//
//    def regions: Set[String] = regionsWithVolumeCountAndSum.keySet
//
//    def updateWith(tuple: (String, Iterable[Avocado])): AggregationResult =
//      updateWith(tuple._1, tuple._2)
//
//    def updateWith(region: String, rdds: Iterable[Avocado]): AggregationResult = if (rdds.nonEmpty) {
//      rdds.foldLeft(this) { case (acc, a) =>
//        acc.copy(
//          regionsWithVolumeCountAndSum.updatedWith(region)(
//            _.map { case (i, d) => (i + 1, d + a.volume) }.orElse(Some((1, a.volume)))
//          ),
//          math.max(maxAvgPrice, a.avgPrice),
//          math.min(minAvgPrice, a.avgPrice),
//          monthCounts.updatedWith(a.localDate.getMonthValue)(_.map(_ + 1).orElse(Some(1)))
//        )
//      }
//    } else this
//
//    def union(that: AggregationResult): AggregationResult =
//      copy(
//        regionsWithVolumeCountAndSum.foldLeft(that.regionsWithVolumeCountAndSum) { case (acc, (k, (c, v))) =>
//          acc.updatedWith(k)(_.map { case (i, d) => (i + c, d + v) }.orElse(Some((c, v))))
//        },
//        math.max(this.maxAvgPrice, that.maxAvgPrice),
//        math.min(this.minAvgPrice, that.minAvgPrice),
//        monthCounts.foldLeft(that.monthCounts) { case (acc, (m, c)) =>
//          acc.updatedWith(m)(_.map(_ + c).orElse(Some(c)))
//        }
//      )
//  }
//
//  val xDay = LocalDate.of(2018, 2, 11)
//
//  val salesAfterXDay = avocadoRDD.filter(_.localDate.isAfter(xDay))
//  val aggregationResult = avocadoRDD
//    .groupBy(_.region)
//    .aggregate(AggregationResult(Map.empty, Double.MinPositiveValue, Double.MaxValue, Map.empty))(
//      _.updateWith(_),
//      _.union(_)
//    )
//
//
//  println("salesAfterXDay:")
//  salesAfterXDay.foreach(println)
//
//  println("regions count:   " ++ aggregationResult.regions.size.toString)
//  println("maxFrequentMonth:" ++ aggregationResult.maxFrequentMonth.getOrElse(-1).toString)
//  println("maxAvgPrice:     " ++ aggregationResult.maxAvgPrice.toString)
//  println("minAvgPrice:     " ++ aggregationResult.minAvgPrice.toString)
//  println("avgVolumes:      ")
//  aggregationResult.avgVolumesByRegions.foreach { case (str, d) => println(str ++ ":" ++ d.toString) }
//
//}
