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
        val username = (json \ "username").as[String]
        val password = (json \ "password").as[String]
        JsSuccess(Login(username, password))
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
        val userType = (json \ "user_type").as[String]
        JsSuccess(LoginSuccess(userType))
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
        val seq = (json \ "seq").as[Int]
        JsSuccess(Ping(seq))
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
        val seq = (json \ "seq").as[Int]
        JsSuccess(Pong(seq))
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
  case class Table(id: Int, name: String, participants: Int) {
    override def equals(obj: scala.Any): Boolean = {
      if (obj.isInstanceOf[Table]) {
        if (id == obj.asInstanceOf[Table].id) true else false
      } else {
        false
      }
    }
  }
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
        val id = (json \ "id").as[Int]
        val name = (json \ "name").as[String]
        val participants = (json \ "participants").as[Int]
        JsSuccess(Table(id, name, participants))
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
        val tableList = (json \\ "tables").map(jsvalue => jsvalue.as[Table]).toList
        JsSuccess(TableList(tableList))
      }
    }
  }
  case class UnsubscribeTables() extends Msg("unsubscribe_tables")
  object UnsubscribeTables {
    implicit object UnsubscribeTablesFormat extends Format[UnsubscribeTables] {
      override def writes(o: UnsubscribeTables): JsValue = {
        val seq = Seq(
          "$type" -> JsString(o.$type)
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[UnsubscribeTables] = {
        JsSuccess(UnsubscribeTables())
      }
    }
  }
  case class NotAuthorized() extends Msg("not_authorized")
  object NotAuthorized {
    implicit object NotAuthorizedFormat extends Format[NotAuthorized] {
      override def writes(o: NotAuthorized): JsValue = {
        val seq = Seq(
          "$type" -> JsString(o.$type)
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[NotAuthorized] = {
        JsSuccess(NotAuthorized())
      }
    }
  }
  case class AddTable(after_id: Int, table: Table) extends Msg("add_table")
  object AddTable {
    implicit object AddTableFormat extends Format[AddTable] {
      override def writes(o: AddTable): JsValue = {
        val seq = Seq(
          "$type" -> JsString(o.$type),
          "after_id" -> JsNumber(o.after_id),
          "table" -> Json.toJson(o.table)
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[AddTable] = {
        val afterId = (json \ "after_id").as[Int]
        val table = (json \ "table").as[Table]
        JsSuccess(AddTable(afterId, table))
      }
    }
  }
  case class TableAdded(id: Int, name: String, participants: Int) extends Msg("table_added")
  object TableAdded {
    implicit object TableAddedFormat extends Format[TableAdded] {
      override def writes(o: TableAdded): JsValue = {
        val seq = Seq(
          "$type" -> JsString(o.$type),
          "id" -> JsNumber(o.id),
          "name" -> Json.toJson(o.name),
          "participants" -> JsNumber(o.participants)
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[TableAdded] = {
        val id = (json \ "id").as[Int]
        val name = (json \ "name").as[String]
        val participants = (json \ "participants").as[Int]
        JsSuccess(TableAdded(id, name, participants))
      }
    }
  }
  case class UpdateTable(table: Table) extends Msg("update_table")
  object UpdateTable {
    implicit object UpdateTableFormat extends Format[UpdateTable] {
      override def writes(o: UpdateTable): JsValue = {
        val seq = Seq(
          "$type" -> JsString(o.$type),
          "table" -> Json.toJson(o.table)
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[UpdateTable] = {
        val table = (json \ "table").as[Table]
        JsSuccess(UpdateTable(table))
      }
    }
  }
  case class TableUpdated(table: Table) extends Msg("table_updated")
  object TableUpdated {
    implicit object TableUpdatedFormat extends Format[TableUpdated] {
      override def writes(o: TableUpdated): JsValue = {
        val seq = Seq(
          "$type" -> JsString(o.$type),
          "table" -> Json.toJson(o.table)
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[TableUpdated] = {
        val table = (json \ "table").as[Table]
        JsSuccess(TableUpdated(table))
      }
    }
  }
  case class RemoveTable(id: Int) extends Msg("remove_table")
  object RemoveTable {
    implicit object RemoveTableFormat extends Format[RemoveTable] {
      override def writes(o: RemoveTable): JsValue = {
        val seq = Seq(
          "$type" -> JsString(o.$type),
          "id" -> JsNumber(o.id)
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[RemoveTable] = {
        val id = (json \ "id").as[Int]
        JsSuccess(RemoveTable(id))
      }
    }
  }
  case class TableRemoved(id: Int) extends Msg("table_removed")
  object TableRemoved {
    implicit object TableRemovedFormat extends Format[TableRemoved] {
      override def writes(o: TableRemoved): JsValue = {
        val seq = Seq(
          "$type" -> JsString(o.$type),
          "id" -> JsNumber(o.id)
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[TableRemoved] = {
        val id = (json \ "id").as[Int]
        JsSuccess(TableRemoved(id))
      }
    }
  }
  case class RemovalFailed(id: Int) extends Msg("removal_failed")
  object RemovalFailed {
    implicit object RemovalFailedFormat extends Format[RemovalFailed] {
      override def writes(o: RemovalFailed): JsValue = {
        val seq = Seq(
          "$type" -> JsString(o.$type),
          "id" -> JsNumber(o.id)
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[RemovalFailed] = {
        val id = (json \ "id").as[Int]
        JsSuccess(RemovalFailed(id))
      }
    }
  }
  case class UpdateFailed(id: Int) extends Msg("update_failed")
  object UpdateFailed {
    implicit object UpdateFailedFormat extends Format[UpdateFailed] {
      override def writes(o: UpdateFailed): JsValue = {
        val seq = Seq(
          "$type" -> JsString(o.$type),
          "id" -> JsNumber(o.id)
        )
        JsObject(seq)
      }

      override def reads(json: JsValue): JsResult[UpdateFailed] = {
        val id = (json \ "id").as[Int]
        JsSuccess(UpdateFailed(id))
      }
    }
  }

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
        case "add_table" => {
          val addTable = json.as[AddTable]
          val id = addTable.after_id
          val table = addTable.table
          listOfTables += Table(id, table.name, table.participants)
          Json.toJson(TableAdded(id, table.name, table.participants)).toString
        }
        case "update_table" => {
          val tableToUpdate = json.as[UpdateTable]
          val idx = listOfTables.indexOf(tableToUpdate.table)
          if (idx >= 0) {
            listOfTables(idx) = tableToUpdate.table
            Json.toJson(TableUpdated(tableToUpdate.table)).toString
          } else {
            Json.toJson(UpdateFailed(tableToUpdate.table.id)).toString
          }
        }
        case "remove_table" => {
          val tableToRemove = json.as[RemoveTable]
          if (tableToRemove.id >= 0 && tableToRemove.id < listOfTables.size) {
            listOfTables.remove(tableToRemove.id)
            Json.toJson(TableRemoved(tableToRemove.id)).toString
          } else {
            Json.toJson(RemovalFailed(tableToRemove.id)).toString
          }
        }
        case _ => Json.toJson(LoginFailed()).toString
      }
    } catch {
      case e: Exception =>
        s"""{"error":"${e.getMessage}"}"""
    }

}
