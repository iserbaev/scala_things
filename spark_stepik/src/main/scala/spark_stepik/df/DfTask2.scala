package spark_stepik.df

import org.apache.spark.sql.functions._
import org.apache.spark.sql.{ DataFrame, SaveMode }
import spark_stepik.SparkCxt

object DfTask2 extends App with SparkCxt {

  def extractWordsInLowerCase(wordAlias: String)(df: DataFrame): DataFrame =
    df
      .select(
        explode_outer(split(lower(col("_c0")), "\\W+"))
          .as(wordAlias)
      )
      .filter(col(wordAlias) =!= "")

  def extractWordFrequencies(wordAlias: String, countAlias: String)(df: DataFrame): DataFrame =
    df
      .groupBy(wordAlias)
      .agg(count(col(wordAlias)).as(countAlias))
      .orderBy(desc(countAlias))
      .withColumn("id", monotonically_increasing_id())
      .limit(20)

  def getJoinedStats(joinDf: DataFrame)(df: DataFrame): DataFrame =
    joinDf
      .join(df, joinDf.col("id") === df.col("id"))
      .drop(df.col("id"))

  def read(file: String, wordAlias: String, countAlias: String) =
    spark.read
      .option("inferSchema", "true")
      .csv(file)
      .transform(extractWordsInLowerCase(wordAlias))
      .transform(extractWordFrequencies(wordAlias, countAlias))

  val subtitlesS1DF =
    read("spark_stepik/src/main/resources/1_df_files/subtitles_s1.json", "w_s1", "cnt_s1")
  val subtitlesS2DF =
    read("spark_stepik/src/main/resources/1_df_files/subtitles_s2.json", "w_s2", "cnt_s2")

  val joinedSubtitlesS1S2DF = getJoinedStats(subtitlesS1DF)(subtitlesS2DF)

  joinedSubtitlesS1S2DF.write
    .mode(SaveMode.Overwrite)
    .save("spark_stepik/src/main/resources/data/wordcount")

}
