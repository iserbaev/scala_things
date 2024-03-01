//package spark_stepik.ds
//
//import org.apache.spark.sql.DataFrame
//import org.apache.spark.sql.functions._
//import spark_stepik.{ SparkCxt, replaceNull }
//
//object AIJobsIndustryDS extends App with SparkCxt {
//  val aiJobsIndustryDF: DataFrame = spark.read
//    .option("header", "true")
//    .option("inferSchema", "true")
//    .option("multiLine", "true")
//    .csv("spark_stepik/src/main/resources/2_ds_files/AIJobsIndustry.csv")
//
//  def extractColumns(df: DataFrame): DataFrame =
//    df.select(
//      replaceNull("JobTitle", lit("n/a")).as("Job"),
//      regexp_replace(
//        replaceNull("Company", lit("n/a")),
//        "[\\n ]",
//        ""
//      ).as("Company"),
//      replaceNull("Location", lit("n/a")),
//      regexp_replace(
//        replaceNull("CompanyReviews", lit(0)),
//        "[^0-9]+",
//        ""
//      ).as("CompanyReviews"),
//      replaceNull("Link", lit("n/a")),
//      regexp_extract(col("Link"), "fccid=([^']*)", 0).as("fccid")
//    ).dropDuplicates("fccid")
//
//  val cleanedDF = aiJobsIndustryDF.transform(extractColumns)
//
//  cleanedDF.show(10)
//
//  /* Dataset*/
//
//  case class JobsInfo(
//      jobTitle: String,
//      company: String,
//      location: String,
//      companyReviews: String,
//      link: String,
//      fccid: String
//  ) {
//    def companyReviewsNum = companyReviews.toIntOption.getOrElse(0)
//  }
//  case class JobsStat(name: String, statsType: String, location: String, count: Int, countType: String)
//  case class JobsStats(min: JobsStat, max: JobsStat, sum: Int)
//
//}
