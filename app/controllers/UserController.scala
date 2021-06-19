package controllers

import models.UserModel
import play.api.db.Database

import javax.inject._
import play.api.mvc._
/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class UserController @Inject()(val db: Database, val controllerComponents: ControllerComponents) extends BaseController {

  def getUser(request: Request[AnyContent]): UserModel = {
    new UserModel(
      name = request.session.get("name").getOrElse(""),
      surname = request.session.get("surname").getOrElse(""),
      email = request.session.get("email").getOrElse(""),
      isAdmin = request.session.get("isAdmin").getOrElse("false").toBoolean
    )
  }

  def login: Action[AnyContent] = Action { request =>
    val message = request.flash.get("login").getOrElse("")
    if (request.session.get("email").isDefined)
      Redirect(routes.UserController.profile())
    else {
      implicit val userInfo: UserModel = new UserModel()
      Ok(views.html.auth(message))
    }
  }

  def signUp(): Action[AnyContent] = Action { request =>
    val message = request.flash.get("signUp").getOrElse("")
    if (request.session.get("email").isEmpty) {
      implicit val userInfo: UserModel = new UserModel()
      Ok(views.html.signup(message))
    }
    else
      Redirect(routes.UserController.profile())
  }

  def createUser(): Action[AnyContent] = Action { request =>
    val postValues = request.body.asFormUrlEncoded
    postValues.map { args =>
      val email = args("email").head
      val password = args("password").head
      val repeatedPass = args("repeatedPassword").head
      val name = args("name").head
      val surname = args("surname").head
      val mess = models.UserModel.createUser(db, email, name, surname, password, repeatedPass)
      if (mess == "") {
        Redirect(routes.HomeController.myEquip(1, "")).withSession("email" -> email, "name" -> name, "surname" -> surname, "isAdmin" -> "false").flashing("signUp" -> mess)
      } else
        Redirect(routes.UserController.signUp()).flashing("signUp" -> mess)
    }.getOrElse(BadRequest)
  }

  def validUser(): Action[AnyContent] = Action { request =>
    val postValues = request.body.asFormUrlEncoded
    postValues.map { args =>
      val email = args("email").head
      val password = args("password").head
      val validResult: (String, UserModel) = models.UserModel.validUser(db, email, password)
      val message = validResult._1
      val userInfo = validResult._2
      if (message == "") {
        Redirect(routes.HomeController.myEquip(1, "")).withSession("email" -> userInfo.email, "name" -> userInfo.name, "surname" -> userInfo.surname, "isAdmin" -> userInfo.isAdmin.toString)
      } else {
        Redirect(routes.UserController.login()).flashing("login" -> message)
      }
    }.getOrElse(BadRequest)
  }

  def changePass(): Action[AnyContent] = Action { request =>
    val message = request.flash.get("changePass").getOrElse("")
    if (request.session.get("email").isDefined) {
      implicit val userInfo: UserModel = models.UserModel.getUser(db, request.session.get("email").get)
      Ok(views.html.changePass(message))
    } else
      Redirect(routes.UserController.login())
  }

  def changePassPost(): Action[AnyContent] = Action { request =>
    val postValues = request.body.asFormUrlEncoded
    postValues.map { args =>
      val password = args("password").head
      val newPass = args("newPass").head
      val newPassAgain = args("newPassAgain").head
      val message = models.UserModel.changePass(db, request.session.get("email").get, password, newPass, newPassAgain)
      Redirect(routes.UserController.changePass()).flashing("changePass" -> message)
    }.getOrElse(Ok("Error"))
  }

  def editProfilePost(): Action[AnyContent] = Action { request =>
    val postValues = request.body.asFormUrlEncoded
    postValues.map { args =>
      val name = args("name").head
      val surname = args("surname").head
      val message = models.UserModel.editProfile(db, request.session.get("email").get, name, surname)
      val userInfo = models.UserModel.getUser(db, request.session.get("email").getOrElse(""))
      Redirect(routes.UserController.profile()).withSession("email" -> userInfo.email, "name" -> userInfo.name, "surname" -> userInfo.surname, "isAdmin" -> userInfo.isAdmin.toString).flashing("editProfile" -> message)
    }.getOrElse(Ok("Error"))
  }

  def logout(): Action[AnyContent] = Action { request =>
    Redirect(routes.UserController.login()).withNewSession.flashing("logout" -> "success")
  }

  def profile(): Action[AnyContent] = Action { implicit request =>
    val message = request.flash.get("editProfile").getOrElse("")
    if (request.session.get("email").isDefined) {
      implicit val userInfo: UserModel = getUser(request)
      Ok(views.html.profile(message))
    }
    else
      Redirect(routes.UserController.login())
  }
}