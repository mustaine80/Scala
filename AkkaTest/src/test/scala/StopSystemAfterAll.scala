import akka.testkit.TestKit
import org.scalatest.{BeforeAndAfterAll, Suite}

//  mix-in this trait with test actor using TestKit
//  this support TestKit supporting system shutdown after running all tests
trait StopSystemAfterAll extends BeforeAndAfterAll {
  this: TestKit with Suite =>
  override protected def afterAll(): Unit = {
    super.afterAll()
    system.terminate()
  }
}
