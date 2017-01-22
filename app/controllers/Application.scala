package controllers

import play.api.libs.iteratee.{Concurrent, Iteratee}
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.{JsValue, _}

class Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def echo = WebSocket.using[String] { request =>
    // Concurrent.broadcast returns (Enumerator, Concurrent.Channel)
    val (out, channel) = Concurrent.broadcast[String]

    // log the message to stdout and send response back to client
    val in = Iteratee.foreach[String] {
      msg => println(msg)
        // the Enumerator returned by Concurrent.broadcast subscribes to the channel and will
        // receive the pushed messages
        channel push(msg)
    }

    (in, out)
  }

  def socket = WebSocket.using[String] { request =>
    // Concurrent.broadcast returns (Enumerator, Concurrent.Channel)
    val (out, channel) = Concurrent.broadcast[String]
    // parse the message and send response back to client
    val in = Iteratee.foreach[String] {
      msg => channel push dispatchCommand(msg)
    }

    (in, out)
  }

  private def dispatchCommand(msg: String) =
    try {
      implicit val modelFormat = Json.format[Model]

      // parse message into Json and convert it to model object
      val json = Json.parse(msg)
      val model = modelFormat.reads(json).asOpt
      // create a reply Json
      if (model.isDefined) {
        Json.toJson(Model("my friend " + model.get.hello, model.get.age + 11)).toString
      } else {
        """{"error":"Wrong json object."}"""
      }
    } catch {
      case e: Exception =>
        s"""{"error":"${e.getMessage}"}"""
    }


  case class Model(hello: String, age: Int)

  def tables = WebSocket.using[String] { request =>
    // Concurrent.broadcast returns (Enumerator, Concurrent.Channel)
    val (out, channel) = Concurrent.broadcast[String]
    // parse the message and send response back to client
    val in = Iteratee.foreach[String] {
      msg => channel push dispatchTableCommand(msg)
    }

    (in, out)
  }

  class Msg(val $type: String)
  object Msg {
    implicit object MsgFormat extends Format[Msg] {
      override def writes(o: Msg): JsValue = {
        val seq = Seq(
          "$type" -> JsString(o.$type)
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[Msg] = {
        JsSuccess(new Msg(""))
      }
    }
  }
  case class Login(username: String, password: String) extends Msg("login")
  object Login {
    implicit object LoginFormat extends Format[Login] {
      override def writes(o: Login): JsValue = {
        val seq = Seq(
          "$type" -> JsString(o.$type),
          "username" -> JsString(o.username),
          "password" -> JsString(o.password)
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[Login] = {
        JsSuccess(Login("", ""))
      }
    }
  }
  case class LoginSuccess(user_type: String = "admin") extends Msg("login_successful")
  object LoginSuccess {
    implicit object LoginSuccessFormat extends Format[LoginSuccess] {
      override def writes(o: LoginSuccess): JsValue = {
        val seq = Seq(
          "$type" -> JsString(o.$type),
          "user_type" -> JsString(o.user_type)
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[LoginSuccess] = {
        JsSuccess(LoginSuccess("admin"))
      }
    }
  }
  case class LoginFailed() extends Msg("login_failed")
  object LoginFailed {
    implicit object LoginFailedFormat extends Format[LoginFailed] {
      override def writes(o: LoginFailed): JsValue = {
        val seq = Seq(
          "$type" -> JsString(o.$type)
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[LoginFailed] = {
        JsSuccess(LoginFailed())
      }
    }
  }
  case class Ping(seq: Int = 1) extends Msg("ping")
  object Ping {
    implicit object PingFormat extends Format[Ping] {
      override def writes(o: Ping): JsValue = {
        val seq = Seq(
          "$type" -> JsString(o.$type),
          "seq" -> JsNumber(o.seq)
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[Ping] = {
        JsSuccess(Ping())
      }
    }
  }
  case class Pong(seq: Int = 1) extends Msg("pong")
  object Pong {
    implicit object PongFormat extends Format[Pong] {
      override def writes(o: Pong): JsValue = {
        val seq = Seq(
          "$type" -> JsString(o.$type),
          "seq" -> JsNumber(o.seq)
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[Pong] = {
        JsSuccess(Pong())
      }
    }
  }
  case class SubscribeTables() extends Msg("subscribe_tables")
  object SubscribeTables {
    implicit object SubscribeTablesFormat extends Format[SubscribeTables] {
      override def writes(o: SubscribeTables): JsValue = {
        val seq = Seq(
          "$type" -> JsString(o.$type)
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[SubscribeTables] = {
        JsSuccess(SubscribeTables())
      }
    }
  }
  case class Table(id: Int, name: String, participants: Int)
  object Table {
    implicit object TableFormat extends Format[Table] {
      override def writes(o: Table): JsValue = {
        val seq = Seq(
          "id" -> JsNumber(o.id),
          "name" -> JsString(o.name),
          "participants" -> JsNumber(o.participants)
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[Table] = {
        JsSuccess(Table(0, "", 0))
      }
    }
  }
  case class TableList(tables: List[Table]) extends Msg("table_list")
  object TableList {
    implicit object TableListFormat extends Format[TableList] {
      override def writes(o: TableList): JsValue = {
        val seq = Seq(
          "$type" -> JsString(o.$type),
          "tables" -> JsArray(o.tables.map(table => Json.toJson(table)))
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[TableList] = {
        JsSuccess(TableList(List[Table]()))
      }
    }
  }
  case class UnsubscribeTables() extends Msg("unsubscribe_tables")
  case class NotAuthorized() extends Msg("not_authorized")
  case class AddTable(after_id: Int, table: Table) extends Msg("add_table")
  case class TableAdded(after_id: Int, table: Table) extends Msg("table_added")
  case class UpdateTable(table: Table) extends Msg("update_table")
  case class TableUpdated(table: Table) extends Msg("table_updated")
  case class RemoveTable(id: Int) extends Msg("remove_table")
  case class TableRemoved(id: Int) extends Msg("table_removed")
  case class RemovalFailed(id: Int) extends Msg("removal_failed")
  case class UpdateFailed(id: Int) extends Msg("update_failed")

  val listOfTables = new scala.collection.mutable.ArrayBuffer[Table]()
  listOfTables += Table(1,  "table - James Bond", 7)
  listOfTables += Table(2,  "table - Mission Impossible", 4)

  private def dispatchTableCommand(message: String) =
    try {
      // parse message into Json and convert it to Msg object
      val json = Json.parse(message)
      (json \ "$type").asOpt[String].get match {
        case "login" => Json.toJson(LoginSuccess()).toString
        case "ping" => Json.toJson(Pong()).toString
        case "subscribe_tables" => Json.toJson(TableList(listOfTables.toList)).toString
        case _ => Json.toJson(LoginFailed()).toString
      }
    } catch {
      case e: Exception =>
        s"""{"error":"${e.getMessage}"}"""
    }

}
