import akka.actor.{ActorRef, ActorSystem, Inbox, Props}
import scala.concurrent.duration._
/**
  * Created by ilnur on 15.11.16.
  */
object App extends App{
  val base1 = ActorSystem("firstBase")

  val actor11 = base1.actorOf(Props[Actor1], "actor1base1")
  val actor21 = base1.actorOf(Props[Actor2], "actor2base1")

//  actor11 ! StartMsgTo(actor21)

  val inbox1 = Inbox.create(base1)

  actor11.tell("message of actor1base1 to actor2base2", actor21)
  println("actor11 tell")

  val sendOb11 = inbox1.send(actor11, StartMsgTo(actor21))
  println("sendOb actor11")

}
