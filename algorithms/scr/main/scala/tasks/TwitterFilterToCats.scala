package tasks

import cats.data.{Kleisli, ReaderT}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object TwitterFilterToCats extends App {
  type Service[Req, Resp] = Req                       => Future[Resp]
  type Filter[Req, Resp]  = (Req, Service[Req, Resp]) => Future[Resp]

  val sT: Service[String, String] = Future.successful
  val fT: Filter[String, String]  = (r, s) => s(r.toUpperCase)

  type CatsService[Req, Resp] = ReaderT[Future, Req, Resp]
  type CatsFilter[Req, Resp]  = (Req, CatsService[Req, Resp]) => Future[Resp]
  val sC: CatsService[String, String] = Kleisli(Future.successful)
  val fC: CatsFilter[String, String]  = (r, k) => k.run(r.toUpperCase)

  val s = "low"

  val resultT = Await.result(fT(s, sT), Duration.Inf)
  val resultC = Await.result(fC(s, sC), Duration.Inf)
  println(resultT)
  println(resultC)

  assert(resultT == resultC)

}
