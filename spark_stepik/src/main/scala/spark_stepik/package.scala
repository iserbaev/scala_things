import org.apache.spark.sql.Column
import org.apache.spark.sql.functions._

package object spark_stepik {
  def replaceNull(columnName: String, column: Column): Column =
    coalesce(col(columnName), column).as(columnName)

}
