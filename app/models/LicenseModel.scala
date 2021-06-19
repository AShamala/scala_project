package models

import play.api.db.Database

case class LicenseModel(id: Int, name: String, file: String, dateEnd: String, isExpired: Boolean)

object LicenseModel {

  def getLicenseCount(db: Database, search: String): Int = {
    db.withConnection { conn =>
      val query = conn.createStatement().executeQuery(s"SELECT COUNT(*) FROM LICENSE WHERE UPPER(license_name) LIKE UPPER('%$search%')")
      query.next()
      query.getInt(1)
    }
  }

  def findLicense(db: Database, file: String): Boolean = {
    db.withConnection { conn =>
      val res = conn.createStatement().executeQuery(s"SELECT * FROM License WHERE file_name='$file'")
      res.next()
    }
  }

  def addLicense(db: Database, file: String, name: String, dateEnd: String): String = {
    db.withConnection { conn =>
      if (name != "" && name.length < 255 && file.endsWith(".pdf") && dateEnd != "") {
        conn.createStatement().executeUpdate(s"INSERT INTO License (license_name,file_name,date_end) VALUES ('$name','$file','$dateEnd')")
        "Файл добавлен"
      } else if (name == "") "Имя не должно быть пустым"
      else if (dateEnd == "") "Укажите дату"
      else "Файл не в формате pdf"
    }
  }

  def searchLicenses(db: Database, page: Int, search: String): List[LicenseModel] = {
    var result: List[LicenseModel] = List()
    db.withConnection { conn =>
      val query = conn.createStatement().executeQuery(s"SELECT * FROM License WHERE UPPER(license_name) LIKE UPPER('%$search%') LIMIT 6 OFFSET ${(page - 1) * 6}")
      while (query.next()) {
        val dateEnd = query.getString("date_end")
        val parsedDate = dateEnd.split("-").map(x => x.toInt)
        val dateNow = java.time.LocalDate.now
        val isExpired = {
          if (parsedDate(0) - dateNow.getYear > 0) false
          else if (parsedDate(0) - dateNow.getYear == 0 && parsedDate(1) - dateNow.getMonth.getValue > 0) false
          else if (parsedDate(1) - dateNow.getMonth.getValue == 0 && parsedDate(2) - dateNow.getDayOfMonth > 0) false
          else true
        }
        result = result.appended(
          new LicenseModel(
            query.getInt("license_id"),
            query.getString("license_name"),
            query.getString("file_name"),
            dateEnd,
            isExpired
          )
        )
      }
    }
    result
  }
  def deleteLicense(db: Database, id: Int): Int = {
    db.withConnection { conn =>
      conn.createStatement().executeUpdate(s"DELETE FROM License WHERE license_id='$id'")
    }
  }
}
