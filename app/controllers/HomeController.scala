package controllers

import models.UserModel
import javax.inject._
import play.api.mvc._
import play.api.db.Database
import java.io.File
import java.nio.file.Paths

@Singleton
class HomeController @Inject()(val db: Database, val controllerComponents: ControllerComponents) extends BaseController {

  def getUser(request: Request[AnyContent]): UserModel = {
    new UserModel(
      name = request.session.get("name").getOrElse(""),
      surname = request.session.get("surname").getOrElse(""),
      email = request.session.get("email").getOrElse(""),
      isAdmin = request.session.get("isAdmin").getOrElse("false").toBoolean
    )
  }

  def index(): Action[AnyContent] = Action { request =>
    Redirect(routes.HomeController.equip(1, ""))
  }

  def equip(page: Int, search: String = ""): Action[AnyContent] = Action { request =>
    implicit val userInfo: UserModel = getUser(request)
    val query = models.EquipModel.searchEquipment(db, page, search)
    val equipCount = models.EquipModel.equipCount(db, search)
    val pageCount = if (equipCount % 10 == 0) equipCount / 10 else equipCount / 10 + 1
    Ok(views.html.equip(page, pageCount, equipCount, search, query))
  }

  def searchEquipment(): Action[AnyContent] = Action { request =>
    val postValues = request.body.asFormUrlEncoded
    postValues.map { args =>
      val pattern = args("pattern").head
      if (request.headers("referer").split("/")(3).startsWith("my"))
        Redirect(routes.HomeController.myEquip(1, pattern))
      else
        Redirect(routes.HomeController.equip(1, pattern))
    }.getOrElse(Redirect(routes.HomeController.equip(1, "")))
  }

  def addEquipment(): Action[AnyContent] = Action { request =>
    implicit val userInfo: UserModel = getUser(request)
    if (userInfo.isAdmin) {
      val message = request.flash.get("addEquip").getOrElse("")
      val name = request.flash.get("equipName").getOrElse("")
      val status = request.flash.get("equipStatus").getOrElse("Работает")
      val date = request.flash.get("equipDate").getOrElse("")
      val users = models.UserModel.getAllUsers(db)
      Ok(views.html.addEquip(message, name, status, date, users))
    } else
      Redirect(routes.HomeController.equip(1, ""))
  }

  def addEquipmentPost(): Action[AnyContent] = Action { request =>
    val postValues = request.body.asFormUrlEncoded
    postValues.map { args =>
      val name = args("name").head
      val status = args("status").head
      val dateEnd = args("date").head
      val owner = args("owner").head.split('.')
      val ownerId = try {
        Some(owner(0).toInt)
      } catch {
        case e: Exception => None
      }
      val message: (String, Boolean) = models.EquipModel.addEquipment(db, name, status, dateEnd, ownerId)
      if (message._2)
        Redirect(routes.HomeController.addEquipment()).flashing("addEquip" -> message._1)
      else
        Redirect(routes.HomeController.addEquipment()).flashing("addEquip" -> message._1, "equipName" -> name, "equipStatus" -> status, "equipDate" -> dateEnd)
    }.getOrElse(Ok("Error"))
  }

  def editEquipment(id: Int): Action[AnyContent] = Action { request =>
    implicit val userInfo: UserModel = getUser(request)
    val equip = models.EquipModel.getEquipmentById(db, id)
    if (userInfo.email != "" && userInfo.email == equip.owner.email || userInfo.isAdmin) {
      val message = request.flash.get("editEquip").getOrElse("")
      val users = models.UserModel.getAllUsers(db)
      Ok(views.html.editEquip(message, equip, users))
    } else
      Redirect(routes.HomeController.equip(1, ""))
  }

  def editEquipmentPost(): Action[AnyContent] = Action { request =>
    val postValues = request.body.asFormUrlEncoded
    postValues.map { args =>
      val id = args("id").head.toInt
      val name = args("name").head
      val status = args("status").head
      val dateEnd = args("date").head
      val owner = args("owner").head.split('.')
      val ownerId = try {
        Some(owner(0).toInt)
      } catch {
        case e: Exception => None
      }
      val message = models.EquipModel.editEquipment(db, id, name, status, dateEnd, ownerId)
      Redirect(routes.HomeController.editEquipment(id)).flashing("editEquip" -> message)
    }.getOrElse(Ok("Error"))
  }

  def deleteEquipment(id: Int): Action[AnyContent] = Action { request =>
    val userInfo = getUser(request)
    if (userInfo.isAdmin)
      models.EquipModel.deleteEquipment(db, id)
    Redirect(routes.HomeController.equip(1, ""))
  }

  def myEquip(page: Int, search: String): Action[AnyContent] = Action { request =>
    if (request.session.get("email").isDefined) {
      implicit val userInfo: UserModel = models.UserModel.getUser(db, request.session.get("email").getOrElse(""))
      val query = models.EquipModel.searchUserEquipment(db, search, page, userInfo)
      val equipCount = models.EquipModel.userEquipCount(db, userInfo.id, search)
      val pageCount = if (equipCount % 10 == 0) equipCount / 10 else equipCount / 10 + 1
      Ok(views.html.myEquip(page, pageCount, equipCount, search, query))
    } else
      Redirect(routes.UserController.login())
  }

  def license(page: Int, search: String, delete: Boolean): Action[AnyContent] = Action { request =>
    implicit val userInfo: UserModel = getUser(request)
    val licenses = models.LicenseModel.searchLicenses(db, page, search)
    val licensesCount = models.LicenseModel.getLicenseCount(db, search)
    val pageCount = if (licensesCount % 6 == 0) licensesCount / 6 else licensesCount / 6 + 1
    Ok(views.html.lic(page, pageCount, licensesCount, search, delete, licenses))
  }

  def searchLicenses(): Action[AnyContent] = Action { request =>
    val postValues = request.body.asFormUrlEncoded
    postValues.map { args =>
      val pattern = args("pattern").head
      val delete = args("delete").head.toBoolean
      Redirect(routes.HomeController.license(1, pattern, delete))
    }.getOrElse(Redirect(routes.HomeController.license(1, "", delete = false)))
  }

  def addLicense(): Action[AnyContent] = Action { request =>
    implicit val userInfo: UserModel = getUser(request)
    if (userInfo.isAdmin)
      Ok(views.html.addLic(request.flash.get("addLic").getOrElse("")))
    else
      Redirect(routes.HomeController.license(1, "", delete = false))
  }

  def addLicensePost(): Action[AnyContent] = Action { request =>
    var fileName = ""
    var isAdded = false
    request.body.asMultipartFormData.get
      .file("licenseFile")
      .map { licenseFile =>
        fileName = Paths.get(licenseFile.filename).getFileName.toString
        val absPath = new File(".").getCanonicalPath
        isAdded = models.LicenseModel.findLicense(db, fileName)
        if (fileName.endsWith(".pdf") && !isAdded)
          licenseFile.ref.copyTo(Paths.get(s"$absPath/public/licenses/$fileName"), replace = true)
      }
    val postValues = request.body.asMultipartFormData.get.dataParts
    val name = postValues("name").head
    val date = postValues("date").head
    val message = if (!isAdded)
        models.LicenseModel.addLicense(db, fileName, name, date)
      else "Файл с таким именем уже существует"
    Redirect(routes.HomeController.addLicense()).flashing("addLic" -> message)
  }

  def deleteLicense(id: Int, fileName: String): Action[AnyContent] = Action { request =>
    val userInfo = getUser(request)
    if (userInfo.isAdmin) {
      val delCount = models.LicenseModel.deleteLicense(db, id)
      if (delCount > 0) {
        val absPath = new File(".").getCanonicalPath
        val fileTemp = new File(s"$absPath/public/licenses/$fileName")
        if (fileTemp.exists) {
          fileTemp.delete()
        }
      }
    }
    val ref = request.headers.get("referer").getOrElse("")
    if (ref == "")
      Redirect(routes.HomeController.license(1, "", delete = false))
    else
      Redirect(ref)
  }

  def act(): Action[AnyContent] = Action { request =>
    implicit val userInfo: UserModel = getUser(request)
    Ok(views.html.act())
  }

  def history(id: Int, page: Int, filter: String): Action[AnyContent] = Action { request =>
    implicit val userInfo: UserModel = getUser(request)
    val history = models.RecordModel.getHistory(db, page, id, filter)
    val recordsCount = models.RecordModel.getRecordsCount(db, id, filter)
    val pageCount = if (recordsCount % 10 == 0) recordsCount / 10 else recordsCount / 10 + 1
    val equip = models.EquipModel.getEquipmentById(db, id)
    if (equip.id != -1) {
      Ok(views.html.history(id, page, pageCount, filter, history, equip, request.headers.get("referer").getOrElse("")))
    } else
      Redirect(routes.HomeController.equip(1, ""))
  }

  def historyFilter(): Action[AnyContent] = Action { request =>
    val postValues = request.body.asFormUrlEncoded
    postValues.map { args =>
        val id = args("id").head
        val filter = args("filter").head
        Redirect(routes.HomeController.history(id.toInt, 1, filter))
    }.getOrElse(Redirect(routes.HomeController.history(1, 1, "")))
  }

  def addRecord(id: Int): Action[AnyContent] = Action { request =>
    implicit val userInfo: UserModel = getUser(request)
    val equip = models.EquipModel.getEquipmentById(db, id)
    if (equip.id != -1 && userInfo.email != "" && (userInfo.isAdmin || equip.owner.email == userInfo.email)) {
      val users = models.UserModel.getAllUsers(db)
      val message = request.flash.get("addRecord").getOrElse("")
      Ok(views.html.addRecord(message, users, equip))
    } else
      Redirect(routes.HomeController.history(1, 1, ""))
  }

  def addRecordPost(): Action[AnyContent] = Action { request =>
    val postValues = request.body.asFormUrlEncoded
    postValues.map { args =>
      var message = ""
      val recordType = args("type").head
      val equipId = args("equipId").head.toInt
      val ownerId = args("owner").head.toInt
      if (recordType == "Хранение") {
        val place = args("storagePlace").head
        val dateBegin = args("storageDateBegin").head
        val dateEnd = args("storageDateEnd").head
        message = models.RecordModel.addStorageRecord(db, equipId, recordType, ownerId, place, dateBegin, dateEnd)
      } else if (recordType == "Ремонт") {
        val place = args("repPlace").head
        val reason = args("repReason").head
        val dateBegin = args("repDateBegin").head
        val dateEnd = args("repDateEnd").head
        message = models.RecordModel.addRepairsRecord(db, equipId, recordType, ownerId, place, reason, dateBegin, dateEnd)
      } else if (recordType == "Передача") {
        val owner = args("newOwner").head.split('.')
        val newOwnerId = try {
          Some(owner(0).toInt)
        } catch {
          case e: Exception => None
        }
        val date = args("transferDate").head
        message = models.RecordModel.addTransferRecord(db, equipId, recordType, ownerId, newOwnerId, date)
      } else if (recordType == "Списание") {
        val reason = args("writeOffReason").head
        val date = args("writeOffDate").head
        message = models.RecordModel.addWriteOffRecord(db, equipId, recordType, reason, date, ownerId)
      } else
        message = "Выберете тип записи"
      if (message == "")
        Redirect(routes.HomeController.history(equipId, 1, ""))
      else
        Redirect(routes.HomeController.addRecord(equipId)).flashing("addRecord" -> message)
    }.getOrElse(Redirect(routes.HomeController.history(1, 1, "")))
  }

  def deleteRecord(equipId: Int, recordId: Int): Action[AnyContent] = Action { request =>
    val userInfo = getUser(request)
    val record = models.RecordModel.getRecordById(db, recordId)
    if (userInfo.email != "" && (record.owner.email == userInfo.email || userInfo.isAdmin)) {
      models.RecordModel.deleteRecord(db, recordId)
    }
    Redirect(routes.HomeController.history(equipId, 1, ""))
  }
}
