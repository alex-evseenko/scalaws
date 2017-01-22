import org.junit.runner._
import org.specs2.mutable._
import org.specs2.runner._
import play.api.test.Helpers._
import play.api.test._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
@RunWith(classOf[JUnitRunner])
class ApplicationSpec extends Specification {

  "Application" should {

    "send 404 on a bad request" in new WithApplication{
      route(FakeRequest(GET, "/boum")) must beSome.which (status(_) == NOT_FOUND)
    }

    "render the index page" in new WithApplication{
      val home = route(FakeRequest(GET, "/")).get

      status(home) must equalTo(OK)
      contentType(home) must beSome.which(_ == "text/html")
      contentAsString(home) must contain ("Your new application is ready.")
    }

    "ws echo" in {
      running(TestServer(9000)) {

        val clientInteraction = new ClientInteraction("echo")

        clientInteraction.client.connectBlocking()
        clientInteraction.client.send("Hello Client")

        eventually {
          clientInteraction.messages.contains("Hello Client")
        }
      }
    }

    "ws socket" in {
      running(TestServer(9000)) {

        val clientInteraction = new ClientInteraction("socket")

        clientInteraction.client.connectBlocking()
        clientInteraction.client.send("""{"hello": "world", "age": 10}""")

        eventually {
          clientInteraction.messages.contains("""{"hello":"my friend world","age":21}""")
        }
      }
    }

    "ws tables" in {
      running(TestServer(9000)) {

        val clientInteraction = new ClientInteraction("tables")

        clientInteraction.client.connectBlocking()
        clientInteraction.client.send("""{"$type": "login", "username": "user1234", "password": "password1234"}""")

        eventually {
          clientInteraction.messages.contains("""{"$type":"login_successful","user_type":"admin"}""")
        }
      }
    }

  }
}
