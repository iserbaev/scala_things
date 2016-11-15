import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}

/**
  * Created by ilnur on 15.11.16.
  */

class ActorTest(_system: ActorSystem)
  extends TestKit(_system)
  with ImplicitSender
  with Matchers
  with FlatSpecLike
  with BeforeAndAfterAll {

  def this() = this(ActorSystem("ActorTest"))

  override def afterAll: Unit = {
    system.terminate()
  }

  "An ActorTest" should "be able to set a" in {
    val actor11 = TestActorRef(Props[Actor1])
    actor11 ! Stop
    expectMsgType[String] should be("STOP")
  }
}

