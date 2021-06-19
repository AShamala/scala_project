package models

import play.api.db.Database
import java.sql.Date

case class EquipModel(name: String = null, status: String = null, owner: UserModel = new UserModel(), id: Int = -1, warranty: Date = null, isExpired: Boolean = false)

object EquipModel {

  def equipCount(db: Database, search: String): Int = {
    db.withConnection { conn =>
      val query = conn.createStatement().executeQuery(s"SELECT COUNT(*) FROM EQUIPMENT WHERE UPPER(name) LIKE UPPER('%$search%')")
      query.next()
      query.getInt(1)
    }
  }

  def userEquipCount(db: Database, id: Int, search: String): Int = {
    db.withConnection { conn =>
      val query = conn.createStatement().executeQuery(s"SELECT COUNT(*) FROM EQUIPMENT WHERE UPPER(name) LIKE UPPER('%$search%') AND user_id='$id'")
      query.next()
      query.getInt(1)
    }
  }

  private def isExpire(dateEnd: Date) = {
    val dateNow = Date.valueOf(java.time.LocalDate.now.toString)
    if (dateEnd != null) {
      dateNow.getTime - dateEnd.getTime > 0
    } else
      false
  }

  def searchUserEquipment(db: Database, search: String, page: Int, owner: UserModel): List[EquipModel] = {
    db.withConnection { conn =>
      val queryEquip = conn.createStatement().executeQuery(s"SELECT equip_id, name, status, warranty FROM Equipment WHERE UPPER(name) LIKE UPPER('%$search%') AND user_id='${owner.id}' LIMIT 10 OFFSET ${(page - 1) * 10}")
      var result: List[EquipModel] = List()
      while (queryEquip.next()) {
        val warranty = queryEquip.getDate("warranty")
        result = result.appended(new EquipModel(
          queryEquip.getString("name"),
          queryEquip.getString("status"),
          new UserModel(
            id = owner.id,
            name = owner.name,
            surname = owner.surname,
            email = owner.email,
            isAdmin = owner.isAdmin
          ),
          queryEquip.getInt("equip_id"),
          warranty,
          isExpire(warranty)))
      }
      result
    }
  }

  def searchEquipment(db: Database, page: Int, search: String): List[EquipModel] = {
    db.withConnection { conn =>
      val query = conn.createStatement().executeQuery(s"SELECT * FROM EQUIPMENT JOIN USER ON EQUIPMENT.USER_ID=USER.ID WHERE UPPER(equipment.name) LIKE UPPER('%$search%') LIMIT 10 OFFSET ${(page - 1) * 10}")
      var result: List[EquipModel] = List()
      while (query.next()) {
        val owner = new UserModel(
          id = query.getInt("user.id"),
          email = query.getString("user.user_email"),
          name = query.getString("user.name"),
          surname = query.getString("user.surname"),
          isAdmin = query.getBoolean("user.is_admin"))
          val warranty = query.getDate("warranty")
          result = result.appended(new EquipModel(
            query.getString("equipment.name"),
            query.getString("status"),
            owner,
            query.getInt("equip_id"),
            warranty,
            isExpire(warranty)))
      }
      result
    }
  }

  def getEquipmentById(db: Database, id: Int): EquipModel = {
    db.withConnection { conn =>
      val query = conn.createStatement().executeQuery(s"SELECT user.id,user.user_email,user.name as user_name, user.surname,user.is_admin,equipment.warranty,equipment.name as equip_name, equipment.status, equipment.equip_id FROM Equipment JOIN USER ON EQUIPMENT.USER_ID=USER.ID WHERE equip_id='$id'")
      if (query.next()) {
        val user = new UserModel(
          id = query.getInt("id"),
          email = query.getString("user_email"),
          name = query.getString("user_name"),
          surname = query.getString("surname"),
          isAdmin = query.getBoolean("is_admin"))
        val warranty = query.getDate("warranty")
        new EquipModel(
          query.getString("equip_name"),
          query.getString("status"),
          user,
          query.getInt("equip_id"),
          warranty,
          isExpire(warranty))
      } else
        new EquipModel()
    }
  }

  def addEquipment(db: Database, name: String, status: String, dateEnd: String, ownerIdOption: Option[Int]): (String, Boolean) = {
    db.withConnection { conn =>
      val ownerId = ownerIdOption.getOrElse(-1)
      if (!conn.createStatement().executeQuery(s"SELECT id FROM User WHERE id='$ownerId'").next())
        ("Пользователя не существует", false)
      else {
        if (name != "" && ownerId > 0 && name.length <= 255) {
          if (dateEnd != "")
            conn.createStatement().executeUpdate(s"INSERT INTO Equipment (name,status,warranty,user_id) VALUES ('$name','$status','$dateEnd',$ownerId)")
          else
            conn.createStatement().executeUpdate(s"INSERT INTO Equipment (name,status,warranty,user_id) VALUES ('$name','$status',null,$ownerId)")
          ("Оборудование добавлено", true)
        } else if (name != "" && ownerId <= 0 && name.length <= 255) {
          if (dateEnd != "")
            conn.createStatement().executeUpdate(s"INSERT INTO Equipment (name,status,warranty,user_id) VALUES ('$name','$status','$dateEnd',null)")
          else
            conn.createStatement().executeUpdate(s"INSERT INTO Equipment (name,status,warranty,user_id) VALUES ('$name','$status',null,null)")
          ("Оборудование добавлено", true)
        } else if (name == "" || name.length > 255) ("Название не должно быть пустым", false)
        else ("", false)
      }
    }
  }

  def editEquipment(db: Database, equipId: Int, name: String, status: String, dateEnd: String, ownerIdOption: Option[Int]): String = {
    db.withConnection { conn =>
      val ownerId = ownerIdOption.getOrElse(-1)
      if (!conn.createStatement().executeQuery(s"SELECT id FROM User WHERE id='$ownerId'").next())
        "Пользователя не существует"
      else {
        if (name != "" && name.length <= 255 && ownerId > 0) {
          if (dateEnd != "")
            conn.createStatement().executeUpdate(s"UPDATE Equipment SET name='$name',status='$status',warranty='$dateEnd',user_id=$ownerId WHERE equip_id=$equipId")
          else
            conn.createStatement().executeUpdate(s"UPDATE Equipment SET name='$name',status='$status',warranty=null,user_id=$ownerId WHERE equip_id=$equipId")
          "Информация об оборудовании обновлена"
        } else if (name != "" && name.length <= 255 && ownerId <= 0) {
          if (dateEnd != "")
            conn.createStatement().executeUpdate(s"UPDATE Equipment SET name='$name',status='$status',warranty='$dateEnd',user_id=null WHERE equip_id=$equipId")
          else
            conn.createStatement().executeUpdate(s"UPDATE Equipment SET name='$name',status='$status',warranty=null,user_id=null WHERE equip_id=$equipId")
          "Информация об оборудовании обновлена"
        } else if (name == "" || name.length > 255) "Название не должно быть пустым"
        else ""

      }
    }
  }

  def deleteEquipment(db: Database, id: Int): Int = {
    db.withConnection { conn =>
      conn.createStatement().executeUpdate(s"DELETE FROM Equipment WHERE equip_id='$id'")
    }
  }
}
