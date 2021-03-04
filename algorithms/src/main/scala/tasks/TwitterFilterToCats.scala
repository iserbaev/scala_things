package tasks

import cats.data.{Kleisli, ReaderT}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object TwitterFilterToCats extends App {
  type Service[Req, Resp] = Req                       => Future[Resp]
  type Filter[Req, Resp]  = (Req, Service[Req, Resp]) => Future[Resp]

  val identityT: Service[String, String] = Future.successful
  val printT:    Service[String, Unit]   = s => Future.successful(println(s))
  val sUpT:      Service[String, String] = s => Future.successful(s.toUpperCase)
  val sExT:      Service[String, String] = s => Future.successful(s + "!")
  val fT1:       Filter[String, String]  = (r, s) => sUpT(r).flatMap(s)
  val fT2:       Filter[String, String]  = (r, s) => sExT(r).flatMap(s)
  val rT: Filter[String, Unit] = (r, s) =>
    fT1(r, identityT).flatMap(s2 => fT2(s2, identityT)).flatMap(s)

  type CatsService[Req, Resp] = ReaderT[Future, Req, Resp]
  type CatsFilter[Req, Resp]  = (Req, CatsService[Req, Resp]) => Future[Resp]
  val identityC: CatsService[String, String] = Kleisli(Future.successful)
  val printC: CatsService[String, Unit] = Kleisli(
    s => Future.successful(println(s))
  )
  val sUpC: CatsService[String, String] = Kleisli(
    s => Future.successful(s.toUpperCase)
  )
  val sExC: CatsService[String, String] = Kleisli(
    s => Future.successful(s + "!")
  )
  val fC1: CatsFilter[String, String] = (r, k) => sUpC.run(r).flatMap(k.run)
  val fC2: CatsFilter[String, String] = (r, k) => sExC.run(r).flatMap(k.run)
  val rC: CatsFilter[String, Unit] = (r, k) =>
    fC1(r, identityC).flatMap(fC2(_, identityC)).flatMap(s => k.apply(s))

  def catsServiceToTwitter[Req, Rep](
    cs: CatsService[Req, Rep]
  ): Service[Req, Rep] = cs.apply
  def twTCatsService[Req, Rep](serv: Service[Req, Rep]): CatsService[Req, Rep] =
    Kleisli(serv.apply)
  def catsFilterToTwitter[Req, Rep](
    cf: CatsFilter[Req, Rep]
  ): Filter[Req, Rep] = (req, serv) => cf.apply(req, twTCatsService(serv))

  val s = "low"

  Await.result(rT(s, printT), Duration.Inf)
  Await.result(rC(s, printC), Duration.Inf)

  Await.result(
    catsFilterToTwitter(rC)(s, catsServiceToTwitter(printC)),
    Duration.Inf
  )

}
