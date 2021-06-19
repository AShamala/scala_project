package models

import play.api.db.Database

import java.sql.Date

case class RecordModel(id: Int, recordType: String, place: String = "", dateBegin: Date = null, dateEnd: Date = null, reason: String = "", newOwner: UserModel = new UserModel(), owner: UserModel = new UserModel(), isDeletable: Boolean)

object RecordModel {

  def getRecordsCount(db: Database, equipId: Int, filter: String): Int = {
    db.withConnection { conn =>
      val query = if (filter == "Хранение" || filter == "Передача" || filter == "Ремонт" || filter == "Списание")
        conn.createStatement().executeQuery(s"SELECT COUNT(*) FROM Record WHERE type='$filter' AND equip_id='$equipId'")
      else
        conn.createStatement().executeQuery(s"SELECT COUNT(*) FROM Record WHERE equip_id='$equipId'")
      query.next()
      query.getInt(1)
    }
  }

  private def isDeletable(date: Date): Boolean = {
    if (date != null) {
      val dateNow = Date.valueOf(java.time.LocalDate.now)
      dateNow.getTime - date.getTime / (24 * 60 * 60 * 1000) <= 3
    } else
      false
  }

  def getHistory(db: Database, page: Int, equipId: Int, filter: String): List[RecordModel] = {
    db.withConnection { conn=>
      val query = if (filter != "" && filter == "Хранение" || filter == "Передача" || filter == "Ремонт" || filter == "Списание")
        conn.createStatement().executeQuery(s"SELECT * FROM Record WHERE equip_id='$equipId' AND type='$filter' LIMIT 10 OFFSET ${(page - 1) * 10}")
      else
        conn.createStatement().executeQuery(s"SELECT * FROM Record WHERE equip_id='$equipId' LIMIT 10 OFFSET ${(page - 1) * 10}")
      var history: List[RecordModel] = List()
      while (query.next()) {
        val newOwnerQuery = conn.createStatement().executeQuery(s"SELECT * FROM User WHERE id='${query.getInt("new_owner")}'")
        val oldOwnerQuery = conn.createStatement().executeQuery(s"SELECT * FROM User WHERE id='${query.getInt("old_owner")}'")
        val newOwner = if (newOwnerQuery.next()) new UserModel(
          newOwnerQuery.getInt("id"),
          newOwnerQuery.getString("user_email"),
          newOwnerQuery.getString("name"),
          newOwnerQuery.getString("surname"),
          newOwnerQuery.getBoolean("is_admin")
        ) else new UserModel()
        val oldOwner = if (oldOwnerQuery.next()) new UserModel(
          oldOwnerQuery.getInt("id"),
          oldOwnerQuery.getString("user_email"),
          oldOwnerQuery.getString("name"),
          oldOwnerQuery.getString("surname"),
          oldOwnerQuery.getBoolean("is_admin")
        ) else new UserModel()
        history = history.appended(
          new RecordModel(
            query.getInt("record_id"),
            query.getString("type"),
            query.getString("place"),
            query.getDate("date_begin"),
            query.getDate("date_end"),
            query.getString("reason"),
            newOwner,
            oldOwner,
            isDeletable(query.getDate("add_date"))
          )
        )
      }
      history
    }
  }

  def getRecordById(db: Database, id: Int): RecordModel = {
    db.withConnection { conn =>
      val query = conn.createStatement().executeQuery(s"SELECT * FROM Record WHERE record_id='$id'")
      query.next()
      val ownerIds = conn.createStatement().executeQuery(s"SELECT user.id, record.new_owner FROM USER JOIN EQUIPMENT ON USER.id=EQUIPMENT.user_id JOIN RECORD ON EQUIPMENT.equip_id=RECORD.equip_id WHERE record_id='$id'")
      val ownerChecker = ownerIds.next()
      val ownerId = if (ownerChecker) ownerIds.getInt(2)
      val newOwnerId = if (ownerChecker) ownerIds.getInt(1)
      val ownerQuery = conn.createStatement().executeQuery(s"SELECT * FROM User WHERE id='$ownerId'")
      val newOwnerQuery = conn.createStatement().executeQuery(s"SELECT * FROM User WHERE id='$newOwnerId'")
      val owner = if (ownerQuery.next()) {
        new UserModel(
        ownerQuery.getInt("id"),
        ownerQuery.getString("user_email"),
        ownerQuery.getString("name"),
        ownerQuery.getString("surname"),
        ownerQuery.getBoolean("is_admin")
      )} else new UserModel()
      val newOwner = if (newOwnerQuery.next()) new UserModel(
        newOwnerQuery.getInt("id"),
        newOwnerQuery.getString("user_email"),
        newOwnerQuery.getString("name"),
        newOwnerQuery.getString("surname"),
        newOwnerQuery.getBoolean("is_admin")
      ) else new UserModel()
      new RecordModel(
        query.getInt("record_id"),
        query.getString("type"),
        query.getString("place"),
        query.getDate("date_begin"),
        query.getDate("date_end"),
        query.getString("reason"),
        owner,
        newOwner,
        isDeletable(query.getDate("add_date"))
      )
    }
  }

  private def isCorrectDates(dateBegin: String, dateEnd: String): Boolean = {
    if (dateBegin != "" && dateEnd != "") {
      val parsedDateBegin = dateBegin.split("-").map(x => x.toInt)
      val parsedDateEnd = dateEnd.split("-").map(x => x.toInt)
      if (parsedDateEnd(0) - parsedDateBegin(0) > 0) true
      else if (parsedDateEnd(0) - parsedDateBegin(0) == 0 && parsedDateEnd(1) - parsedDateBegin(1) > 0) true
      else if (parsedDateEnd(1) - parsedDateBegin(1) == 0 && parsedDateEnd(2) - parsedDateBegin(2) >= 0) true
      else false
    } else false
  }

  def addStorageRecord(db: Database, id: Int, recordType: String, owner: Int, place: String, dateBegin: String, dateEnd: String): String = {
    db.withConnection { conn =>
      val datesChecker = isCorrectDates(dateBegin, dateEnd)
      if (place != "" && place.length < 255 && dateBegin != "" && dateEnd != "" && datesChecker) {
        val addDate = java.time.LocalDate.now
        conn.createStatement().executeUpdate(s"INSERT INTO Record (type, old_owner, place, date_begin, date_end, equip_id, add_date) VALUES ('$recordType', '$owner', '$place', '$dateBegin', '$dateEnd', '$id', '$addDate')")
        ""
      } else if (place.length >= 255 || !datesChecker) "Недопустимое значение одного из полей"
      else "Заполните все поля"
    }
  }

  def addRepairsRecord(db: Database, id: Int, recordType: String, owner: Int, place: String, reason: String, dateBegin: String, dateEnd: String): String = {
    db.withConnection { conn =>
      val datesChecker = isCorrectDates(dateBegin, dateEnd)
      if (place != "" && place.length < 255 && reason != "" && reason.length < 255 && dateBegin != "" && dateEnd != "" && datesChecker) {
        val addDate = java.time.LocalDate.now
        conn.createStatement().executeUpdate(s"INSERT INTO Record (type, old_owner, place, reason, date_begin, date_end, equip_id, add_date) VALUES ('$recordType', '$owner', '$place', '$reason', '$dateBegin', '$dateEnd', '$id', '$addDate')")
        ""
      } else if (place.length >= 255 || reason.length >= 255 || !datesChecker) "Недопустимое значение одного из полей"
      else "Заполните все поля"
    }
  }

  def addTransferRecord(db: Database, id: Int, recordType: String, oldOwnerId: Int, newOwnerIdOption: Option[Int], date: String): String = {
    db.withConnection { conn =>
      val newOwnerId = newOwnerIdOption.getOrElse(-1)
      val userChecker = if (newOwnerIdOption.isDefined)
          conn.createStatement().executeQuery(s"SELECT id FROM User WHERE id='$newOwnerId'").next()
        else false
      if (userChecker && date != "") {
        val addDate = java.time.LocalDate.now
        conn.createStatement().executeUpdate(s"INSERT INTO Record (type, old_owner, new_owner, date_begin, equip_id, add_date) VALUES ('$recordType', '$oldOwnerId', '$newOwnerId', '$date', '$id', '$addDate')")
        ""
      } else if (!userChecker) "Недопустимое значение одного из полей"
      else "Заполните все поля"
    }
  }

  def addWriteOffRecord(db: Database, id: Int, recordType: String, reason: String, date: String, ownerId: Int): String = {
    db.withConnection { conn =>
      if (reason != "" && reason.length < 255 && date != "") {
        val addDate = java.time.LocalDate.now
        conn.createStatement().executeUpdate(s"INSERT INTO Record (type, reason, date_begin, old_owner, equip_id, add_date) VALUES ('$recordType', '$reason', '$date', '$ownerId', '$id', '$addDate')")
        ""
      } else if (reason.length >= 255) "Недопустимое значение одного из полей"
      else "Заполните все поля"
    }
  }

  def deleteRecord(db: Database, id: Int): Int = {
    db.withConnection { conn =>
      conn.createStatement().executeUpdate(s"DELETE FROM Record WHERE record_id='$id'")
    }
  }
}
