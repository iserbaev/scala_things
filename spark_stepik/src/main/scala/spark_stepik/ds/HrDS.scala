package spark_stepik.ds

import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.functions._
import spark_stepik.SparkCxt

object HrDS extends App with SparkCxt {
  val hrDF: DataFrame = spark.read
    .option("header", "true")
    .option("inferSchema", "true")
    .csv("spark_stepik/src/main/resources/2_ds_files/hrdataset.csv")

  val positionsDF: DataFrame = hrDF
    .select(col("Position"), col("PositionID"))
    .dropDuplicates("PositionID")
    .where(col("Position").isNotNull && col("PositionID").isNotNull)
    .cache()

  def getPositionsByPrefixes(prefixList: List[String]) =
    positionsDF.filter(row => prefixList.exists(row.getAs[String]("Position").contains))

  getPositionsByPrefixes(List("BI", "it")).show()

}
