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

    "ws tables/Login" in {
      running(TestServer(9000)) {

        val clientInteraction = new ClientInteraction("tables")

        clientInteraction.client.connectBlocking()
        clientInteraction.client.send("""{"$type": "login", "username": "user1234", "password": "password1234"}""")
        clientInteraction.client.send("""{"$type": "ping", "seq": 1}""")

        eventually {
          clientInteraction.messages.contains("""{"$type":"login_successful","user_type":"admin"}""")
        }
      }
    }

    "ws tables/Ping" in {
      running(TestServer(9000)) {

        val clientInteraction = new ClientInteraction("tables")

        clientInteraction.client.connectBlocking()
        clientInteraction.client.send("""{"$type": "ping", "seq": 1}""")

        eventually {
          clientInteraction.messages.contains("""{"$type":"pong","seq":1}""")
        }
      }
    }

    "ws tables/TableList" in {
      running(TestServer(9000)) {

        val clientInteraction = new ClientInteraction("tables")

        clientInteraction.client.connectBlocking()
        clientInteraction.client.send("""{"$type": "subscribe_tables"}""")

        eventually {
          clientInteraction.messages.contains("""{"$type":"table_list","tables":[{"id":1,"name":"table - James Bond","participants":7},{"id":2,"name":"table - Mission Impossible","participants":4}]}""")
        }
      }
    }

    "ws tables/AddTable" in {
      running(TestServer(9000)) {

        val clientInteraction = new ClientInteraction("tables")

        clientInteraction.client.connectBlocking()
        clientInteraction.client.send("""{"$type": "add_table", "after_id": 3, "table": {"id": 0, "name": "noname", "participants": 37}}""")

        eventually {
          clientInteraction.messages.contains("""{"$type":"table_added","id":3,"name":"noname","participants":37}""")
        }
      }
    }

  }
}
