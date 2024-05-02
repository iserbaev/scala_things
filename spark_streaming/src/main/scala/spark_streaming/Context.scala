package spark_streaming

import org.apache.spark.sql.SparkSession

trait Context {
  def appName: String = this.getClass.getSimpleName

  lazy val spark: SparkSession = createSession(appName)

  private def createSession(appName: String) =
    SparkSession
      .builder()
      .appName(appName)
      .master("local[*]")
      .getOrCreate()
}
