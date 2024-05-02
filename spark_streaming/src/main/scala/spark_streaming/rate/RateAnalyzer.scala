package spark_streaming.rate

import org.apache.spark.sql.streaming.{ GroupState, GroupStateTimeout, OutputMode }
import org.apache.spark.sql.types.StructType
import org.apache.spark.sql.{ Encoder, Encoders }
import spark_streaming.Context

import java.time.Instant
import java.time.temporal.ChronoUnit

object RateAnalyzer extends App with Context {
  override val appName: String = this.getClass.getSimpleName

  case class Rate(timestamp: Instant, value: Long)
  object Rate {
    implicit val encoder: Encoder[Rate] = Encoders.product[Rate]
    implicit val schema: StructType     = encoder.schema
  }

  case class StateRate(
      key: Instant,
      valueCount: Int,
      totalValue: Long
  )

  case class Measurement(
      value: Instant,
      valueCount: Int,
      totalValue: Long,
      avgValue: Double
  )

  def updateRate(key: Instant, elements: Iterator[Rate], state: GroupState[StateRate]): Iterator[Measurement] = {
    val previousState = if (state.exists) state.get else StateRate(key, 0, 0L)

    val ((currentCount, currentTotalValue), measurementsBuilder) =
      elements.foldLeft(((previousState.valueCount, previousState.totalValue), Iterator.newBuilder[Measurement])) {
        case (((prevCount, prevTotalValue), measurementAcc), rate) =>
          val (currentCount, currentTotalValue) = (prevCount + 1, prevTotalValue + rate.value)

          if (currentCount >= 10) {
            val acc =
              measurementAcc.addOne(
                Measurement(key, currentCount, currentTotalValue, currentTotalValue.toDouble / currentCount)
              )
            ((0, 0L), acc)
          } else {
            ((currentCount, currentTotalValue), measurementAcc)
          }
      }

    state.update(StateRate(key, currentCount, currentTotalValue))
    measurementsBuilder.result()
  }

  import spark.implicits._

  val streamDf = spark.readStream
    .format("rate")
    .option("rowsPerSecond", 3)
    .load()
    .as[Rate]

  val result = streamDf
    .groupByKey(_.timestamp.truncatedTo(ChronoUnit.MINUTES))
    .flatMapGroupsWithState[StateRate, Measurement](
      OutputMode.Append(),
      GroupStateTimeout.NoTimeout()
    )(updateRate)

  result.writeStream
    .format("console")
    .outputMode(OutputMode.Append())
    .option("truncate", "false")
//    .trigger(Trigger.AvailableNow())
    .start()
    .awaitTermination()

}
