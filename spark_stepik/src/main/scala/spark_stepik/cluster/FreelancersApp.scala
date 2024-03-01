package spark_stepik.cluster

import org.apache.log4j.{ Level, Logger }
import org.apache.spark.sql.functions._
import org.apache.spark.sql.{ Column, DataFrame, SparkSession }

//spark_stepik/src/main/resources/6/freelancers.csv
//spark_stepik/src/main/resources/6/offers.csv
object FreelancersApp {
  Logger.getLogger("org").setLevel(Level.ERROR)

  def main(args: Array[String]): Unit = {

    if (args.length != 4) {
      println("Specify the path to the files and num partitions")
      System.exit(1)
    }

    val freelancersCsvPath = args(0)
    val offersCsvPath      = args(1)
    val saltArg            = args(2)
    val joinCase           = args(3).toInt

    val spark = SparkSession
      .builder()
      .appName("Freelancer App2")
      .master("local[*]")
      .getOrCreate()

    val freelancersDF = spark.read
      .option("inferSchema", "true")
      .option("header", "true")
      .csv(freelancersCsvPath)

    val offersDF = spark.read
      .option("inferSchema", "true")
      .option("header", "true")
      .csv(offersCsvPath)

    def avgPriceTransform(left: DataFrame, right: DataFrame)(df: DataFrame) =
      df.filter(abs(left.col("experienceLevel") - right.col("experienceLevel")) <= 1)
        .groupBy("id")
        .agg(avg("price").as("avgPrice"))

    def saltedJoin(
        df: DataFrame,
        buildDf: DataFrame,
        joinExpression: Column,
        joinType: String,
        salt: Int
    ): DataFrame = {
      import org.apache.spark.sql.functions._
      val tmpDf = buildDf.withColumn("slt_range", array(Range(0, salt).toList.map(lit): _*))

      val tableDf  = tmpDf.withColumn("slt_ratio_s", explode(tmpDf("slt_range"))).drop("slt_range")
      val streamDf = df.withColumn("slt_ratio", monotonically_increasing_id() % salt)

      val saltedExpr = streamDf("slt_ratio") === tableDf("slt_ratio_s") && joinExpression

      streamDf.join(tableDf, saltedExpr, joinType).drop("slt_ratio_s").drop("slt_ratio")
    }

    def avgNaiveDF: DataFrame = freelancersDF.join(offersDF, Seq("category", "city")).transform(avgPriceTransform(freelancersDF, offersDF))

    def broadcastJoinDF: DataFrame =
      freelancersDF.join(broadcast(offersDF), Seq("category", "city")).transform(avgPriceTransform(freelancersDF, offersDF))

    def broadcastJoinDF2: DataFrame =
      offersDF.join(broadcast(freelancersDF), Seq("category", "city")).transform(avgPriceTransform(freelancersDF, offersDF))

    def saltedJoinDF: DataFrame = saltedJoin(
      offersDF,
      freelancersDF,
      freelancersDF("category") === offersDF("category") && freelancersDF("city") === offersDF("city"),
      "inner",
      saltArg.toInt
    ).transform(avgPriceTransform(freelancersDF, offersDF))

    def joinDF = joinCase % 4 match {
      case 1 => avgNaiveDF
      case 2 => broadcastJoinDF
      case 3 => broadcastJoinDF2
      case _ => saltedJoinDF
    }

    val result = joinDF
    result.explain()
  }
}
