package models

import play.api.db.Database

case class UserModel(id: Int = 0, email: String = "", name: String = "", surname: String = "", isAdmin: Boolean = false)

object UserModel {
  def validUser(db: Database, email: String, password: String): (String, UserModel) = {
    db.withConnection { conn =>
      val resSet = conn.createStatement().executeQuery(s"SELECT * FROM User WHERE user_email='$email'")
      if (resSet.next() && resSet.getString("password") == password) {
        ("", new UserModel(
          name = resSet.getString("name"),
          surname = resSet.getString("surname"),
          email = resSet.getString("user_email"),
          isAdmin = resSet.getBoolean("is_admin")))
      } else ("Неверный логин или пароль", new UserModel())
    }
  }

  def createUser(db: Database, email: String, name: String, surname: String, password: String, rpass: String): String = {
    db.withConnection { conn =>
      val sameEmail = conn.createStatement().executeQuery(s"SELECT user_email FROM User WHERE user_email='$email'").next()
      val samePass = password == rpass
      val passwordComplexity = isHardPassword(password)
      if (samePass && !sameEmail && passwordComplexity && name != "" && surname != "" && name.length <= 100 && surname.length <= 100 && email != "" && email.length <= 100) {
        conn.createStatement().executeUpdate(s"insert into User (user_email, name, surname, password) values ('$email', '${name.capitalize}', '${surname.capitalize}', '$password')")
        ""
      } else if (email == "" || email.length > 100) "Введите корректный логин"
      else if (sameEmail) "Логин уже занят"
      else if (!samePass) "Пароли не совпадают"
      else if (!passwordComplexity) "Пароль должен содержать не менее 8-ми символов, в том числе цифры, спецсимволы, прописаные и строчные буквы"
      else "Введите корректные имя и фамилию"
    }
  }

  private def isHardPassword(pass: String) = {
    var lowerCase = false
    var upperCase = false
    var number = false
    var symbol = false
    val passLength = pass.length
    pass.foreach { c =>
      if (c.isLetter) {
        if (c.isLower) lowerCase = true
        if (c.isUpper) upperCase = true
      }
      else if (c.isDigit) number = true
      else symbol = true
    }
    lowerCase && upperCase && number && symbol && passLength >= 8 && passLength < 100
  }

  def getUser(db: Database, email: String): UserModel = {
    db.withConnection { conn =>
      val resSet = conn.createStatement().executeQuery(s"SELECT id, user_email, name, surname, is_admin FROM User WHERE user_email='$email'")
      if (resSet.next())
        new UserModel(
          resSet.getInt("id"),
          resSet.getString("user_email"),
          resSet.getString("name"),
          resSet.getString("surname"),
          resSet.getBoolean("is_admin"))
      else
        new UserModel()
    }
  }

  def getAllUsers(db: Database): List[UserModel] = {
    db.withConnection { conn =>
      var result: List[UserModel] = List()
      val resSet = conn.createStatement().executeQuery(s"SELECT id, user_email, name, surname, is_admin FROM User")
      while (resSet.next()) {
        result = result.appended(new UserModel(
          resSet.getInt("id"),
          resSet.getString("user_email"),
          resSet.getString("name"),
          resSet.getString("surname"),
          resSet.getBoolean("is_admin")))
      }
      result
    }
  }

  def changePass(db: Database, email: String, password: String, newPass: String, newPassAgain: String): String = {
    db.withConnection { conn =>
      val sm = conn.createStatement()
      val query = sm.executeQuery(s"SELECT password FROM User WHERE user_email='$email'")
      query.next()
      if (query.getString("password") == password)
        if (newPass == newPassAgain && isHardPassword(newPass)) {
          sm.executeUpdate(s"UPDATE User SET password='$newPass' WHERE user_email='$email'")
          "Пароль изменен"
        } else if (newPass != newPassAgain)
          "Пароли не совпадают"
        else
          "Пароль должен содержать не менее 8-ми символов, в том числе цифры, спецсимволы, прописаные и строчные буквы"
      else
        "Неверный пароль"
    }
  }

  def editProfile(db: Database, email: String, name: String, surname: String): String = {
    db.withConnection { conn =>
      val isNotEmptyName = name != "" && surname != "" && name.length < 100 && surname.length < 100
      if (isNotEmptyName) {
        conn.createStatement().executeUpdate(s"UPDATE User SET name='${name.capitalize}', surname='${surname.capitalize}' WHERE user_email='$email'")
        "Изменения сохранены"
      } else
        "Имя и фамилия не могут быть пустыми"
    }
  }
}
