package spark_stepik.df

import spark_stepik.SparkCxt

object DfDjoins extends App with SparkCxt {
  override val appName: String = "DfDjoins"

  val valuesDF = spark.read
    .option("inferSchema", "true")
    .option("header", "true")
    .csv("spark_stepik/src/main/resources/1_df_files/stack_link_value.csv")

  val tagsDF = spark.read
    .option("inferSchema", "true")
    .option("header", "true")
    .csv("spark_stepik/src/main/resources/1_df_files/stack_links.csv")

  val joinCondition = valuesDF.col("id") === tagsDF.col("key")

  // inner используется по умолчанию, поэтому его можно не указывать
  val innerJoinDF = tagsDF.join(valuesDF, joinCondition)
  val fullOuterDF = tagsDF.join(valuesDF, joinCondition, "outer")
  val leftOuterDF = tagsDF.join(valuesDF, joinCondition, "left_outer")
  val rightOuterDF = tagsDF.join(valuesDF, joinCondition, "right_outer")
  val leftSemiDF = tagsDF.join(valuesDF, joinCondition, "left_semi")

  innerJoinDF.orderBy("id").show(5)
  fullOuterDF.orderBy("id").show(5)
  leftOuterDF.orderBy("id").show(5)
  rightOuterDF.orderBy("id").show(5)
  leftSemiDF.orderBy("key").show(5)

}
