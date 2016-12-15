import org.junit._
import org.junit.Assert._

class MyTest {
  @Test(timeout = 100) def testSomeFeature() {
    assertTrue(6 * 7 == 42)
  }
}
