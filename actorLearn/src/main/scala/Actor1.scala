/**
  * Created by ilnur on 15.11.16.
  */
import akka.actor.{Actor, ActorRef, ActorSystem, Inbox, Props}

case class StartMsgTo(target:ActorRef)
case object Start
case object Ping
case object Pong
case object Stop

class Actor1() extends Actor{
  def receive = {
    case StartMsgTo(target) =>
      println("start ping11")
      target ! Start
    case Ping =>
      println("ping11")
      sender ! Pong
    case Pong =>
      println("pong11")
      sender ! Ping
    case Stop =>
      sender! "STOP"
      println("Stop ")
  }
}

class Actor2 extends Actor{
  def receive = {
    case Start =>
      println("start ping22")
      sender ! Pong
    case Ping =>
      println("ping22")
      sender ! Ping
    case Pong =>
      println("pong22")
      sender ! Stop
  }
}
