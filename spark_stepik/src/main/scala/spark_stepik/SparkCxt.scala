package spark_stepik

import org.apache.spark.SparkContext
import org.apache.spark.sql.SparkSession

trait SparkCxt {

  def appName: String = this.getClass.getSimpleName

  lazy val spark: SparkSession = createSession(appName)
  lazy val sc: SparkContext    = spark.sparkContext

  private def createSession(appName: String) =
    SparkSession
      .builder()
      .appName(appName)
      .master("local[*]")
      .getOrCreate()

}
