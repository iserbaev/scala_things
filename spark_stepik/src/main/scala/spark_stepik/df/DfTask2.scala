package spark_stepik.df

import org.apache.spark.sql.functions._
import org.apache.spark.sql.{ DataFrame, SaveMode }
import spark_stepik.SparkCxt

object DfTask2 extends App with SparkCxt {
  object DfColumn extends DfColumn {
    implicit def columnToString(col: DfColumn.Value): String = col.toString
  }

  trait DfColumn extends Enumeration {
    val _c0, id, w_s1, w_s2, cnt_s1, cnt_s2 = Value
  }

  def read(file: String) =
    spark.read
      .option("inferSchema", "true")
      .csv(file)

  def extractTopWord(wordAlias: String, countAlias: String, wordCount: Int)(df: DataFrame): DataFrame =
    df
      .select(
        explode_outer(split(lower(col(DfColumn._c0)), "\\W+"))
          .as(wordAlias)
      )
      .filter(col(wordAlias) =!= "")
      .groupBy(wordAlias)
      .agg(count(col(wordAlias)).as(countAlias))
      .orderBy(desc(countAlias))
      .limit(wordCount)

  def withId(df: DataFrame) =
    df
      .withColumn(DfColumn.id, monotonically_increasing_id())

  def getJoinedStats(joinDf: DataFrame)(df: DataFrame): DataFrame =
    joinDf
      .join(df, DfColumn.id)

  val subtitlesS1DF =
    read("spark_stepik/src/main/resources/1_df_files/subtitles_s1.json")

  val subtitlesS2DF =
    read("spark_stepik/src/main/resources/1_df_files/subtitles_s2.json")

  val s2TopWordsDF = subtitlesS2DF
    .transform(extractTopWord(DfColumn.w_s2, DfColumn.cnt_s2, 20))
    .transform(withId)

  val topWordsDF = subtitlesS1DF
    .transform(extractTopWord(DfColumn.w_s1, DfColumn.cnt_s1, 20))
    .transform(withId)
    .transform(getJoinedStats(s2TopWordsDF))

  topWordsDF.write
    .mode(SaveMode.Overwrite)
    .save("spark_stepik/src/main/resources/data/wordcount")

  topWordsDF.show(10)

}
