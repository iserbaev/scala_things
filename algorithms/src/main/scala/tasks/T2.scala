package tasks

import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Failure, Success }
import scala.concurrent.duration._

/*
  С помощью кеша написать имплементацию отказоустойчивого выполнения фьючи с кешом
  Если выполнение фьючи падает, пытаемся выдать результат из кеша с признаком того что значение не от фьючи
 */
object ReliableService extends App {

  trait FindCache[V] {
    def find(k: String): Future[Option[V]]
  }

  def reliableCall[V](k: String, f: => Future[V])(
      cache: FindCache[V]
  ): Future[(V, Boolean)] =
    (for {
      res <- f.transform(Success(_))
    } yield {
      res match {
        case Failure(e) =>
          cache
            .find(k)
            .flatMap {
              case Some(value) =>
                Future.successful(value -> false)
              case None =>
                Future.failed(e)
            }
            .transform(_ => Failure(e))
        case Success(value) =>
          Future.successful(value -> true)
      }
    }).flatten

  /*
  Дописать сохранение в кеш в случае удачного вызова
   */

  // решение

  trait Cache[V] {
    def put(k: String, v: V): Future[Unit]

    def find(k: String): Future[Option[V]]
  }

  def reliableCallV2[V](k: String, f: => Future[V])(
      cache: Cache[V]
  ): Future[(V, Boolean)] =
    (for {
      res <- f.transform(Success(_))
    } yield {
      res match {
        case Failure(e) =>
          cache
            .find(k)
            .flatMap {
              case Some(value) =>
                Future.successful(value -> false)
              case None =>
                Future.failed(e)
            }
            .transform {
              case Failure(_)     => Failure(e)
              case Success(value) => Success(value)
            }
        case Success(value) =>
          cache.put(k, value)
          Future.successful(value -> true)
      }
    }).flatten

  class SimpleCache[V] extends Cache[V] {
    private val underlying = new AtomicReference[Map[String, V]](Map.empty)

    override def put(k: String, v: V): Future[Unit] = Future {
      val old = underlying.get()
      underlying.set(old.updated(k, v))
    }

    override def find(k: String): Future[Option[V]] = Future {
      underlying.get().get(k)
    }
  }

  val cache = new SimpleCache[String]
  def test(f: Future[String]): Unit = {
    val fut         = reliableCallV2[String]("ss", f)(cache)
    val result      = Await.result(fut, 1.second)
    val cacheresult = Await.result(cache.find("ss"), 1.second)
    println(cacheresult)
    println(result)
  }

  test(Future("done"))
  test(Future.failed(new RuntimeException("fail")))

}
