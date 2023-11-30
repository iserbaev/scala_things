package spark_stepik.df

import org.apache.spark.sql.{DataFrame, SaveMode}
import org.apache.spark.sql.functions._
import spark_stepik.SparkCxt

object DfTask2 extends App with SparkCxt {
  override val appName: String = "DfTask2"

  def withWordsInLowerCase(wordAlias: String)(df: DataFrame): DataFrame =
    df
      .select(explode_outer(split(col("_c0"), "\\W+")).as(wordAlias))
      .filter(col(wordAlias) =!= "")
      .select(lower(col(wordAlias)).as(wordAlias))

  def withWordFrequencies(wordAlias: String, countAlias: String)(df: DataFrame): DataFrame =
    df
      .groupBy(wordAlias)
      .agg(count(col(wordAlias)).as(countAlias))

  val subtitlesS1DF =
    spark.read
      .option("inferSchema", "true")
      .csv("spark_stepik/src/main/resources/1_df_files/subtitles_s1.json")

  val subtitlesS2DF =
    spark.read
      .option("inferSchema", "true")
      .csv("spark_stepik/src/main/resources/1_df_files/subtitles_s2.json")

  val s1WordCounts = subtitlesS1DF
    .transform(withWordsInLowerCase("w_s1"))
    .transform(withWordFrequencies("w_s1", "cnt_s1"))

  val s2WordCounts = subtitlesS2DF
    .transform(withWordsInLowerCase("w_s2"))
    .transform(withWordFrequencies("w_s2", "cnt_s2"))

  val joinCondition = s1WordCounts.col("w_s1") === s2WordCounts.col("w_s2")

  val orderingColumns = Seq(desc("cnt_s1"), desc("cnt_s2"))
  val joinedSubtitlesS1S2 = s1WordCounts
    .join(s2WordCounts, joinCondition)
    .orderBy(orderingColumns: _*)
    .withColumn("id", monotonically_increasing_id())
    .limit(20)

  joinedSubtitlesS1S2.write
    .mode(SaveMode.Overwrite)
    .save("spark_stepik/src/main/resources/data/wordcount")

}
