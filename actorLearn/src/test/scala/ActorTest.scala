import org.scalatest.{ BeforeAndAfterAll, FlatSpecLike, Matchers }
import akka.actor.{ Actor, Props, ActorSystem }
import akka.testkit.{ ImplicitSender, TestKit, TestActorRef }
import scala.concurrent.duration._

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
    system.shutdown()
    system.awaitTermination(10.seconds)
  }

  "An ActorTest" should "be able to set a new greeting" in {
    val actor11 = TestActorRef(Props[Actor1])
    val actor21 = TestActorRef(Props[Actor2])
    actor11 ! StartMsgTo(actor21)
    actor11.underlyingActor.asInstanceOf[Actor1] should be("hello, testkit")
  }

//  it should "be able to get a new greeting" in {
//    val greeter = system.actorOf(Props[Greeter], "greeter")
//    greeter ! WhoToGreet("testkit")
//    greeter ! Greet
//    expectMsgType[Greeting].message.toString should be("hello, testkit")
//  }
}

