object AppUI {
  def readInput(prompt: String): String = {
    print(prompt)
    scala.io.StdIn.readLine().trim
  }

  def readPassword(prompt: String): String = {
    print(prompt)
    scala.io.StdIn.readLine().trim
  }

  def readConfirmation(prompt: String): Boolean = {
    val answer = readInput(prompt).toLowerCase
    answer == "да" || answer == "yes" || answer == "y"
  }

  def showResult(result: CommandResult): Unit = {
    if (result.output.nonEmpty) {
      println(result.output)
    }
  }

  def createVaultDialog(storagePath: String): Option[UserCommand] = {
    val file = new java.io.File(storagePath)
    if (file.exists()) {
      val answer = readInput(s"Файл '$storagePath' уже существует. Перезаписать? (да/нет): ")
      if (answer.toLowerCase != "да" && answer.toLowerCase != "yes" && answer.toLowerCase != "y") {
        println("Создание отменено пользователем.")
        return None
      }
    }
    println("Создание нового хранилища")
    val pass1 = readPassword("Введите мастер-пароль: ")
    if (pass1.isEmpty) {
      println("Ошибка: Пароль не может быть пустым.")
      return None
    }
    val pass2 = readPassword("Повторите мастер-пароль: ")
    Some(CreateVault(pass1, pass2))
  }

  def openVaultDialog(): Option[UserCommand] = {
    val pass = readPassword("Введите мастер-пароль: ")
    Some(OpenVault(pass))
  }

  def addRecordDialog(): Option[UserCommand] = {
    println("Добавление новой записи")
    println("(обязательные поля отмечены *)")

    val service = readInput("Сервис *: ")
    if (service.isEmpty) {
      println("Ошибка: поле 'Сервис' обязательно для заполнения.")
      return None
    }

    val username = readInput("Имя пользователя *: ")
    if (username.isEmpty) {
      println("Ошибка: поле 'Имя пользователя' обязательно для заполнения.")
      return None
    }

    val password = readInput("Пароль *: ")
    if (password.isEmpty) {
      println("Ошибка: поле 'Пароль' обязательно для заполнения.")
      return None
    }

    val email = readInput("Почта (Enter, если нет): ")
    val url = readInput("Адрес сайта (Enter, если нет): ")
    val note = readInput("Заметка (Enter, если нет): ")

    Some(AddRecord(
      service = service,
      username = username,
      password = password,
      email = if (email.isEmpty) None else Some(email),
      url = if (url.isEmpty) None else Some(url),
      note = if (note.isEmpty) None else Some(note)
    ))
  }

  def editRecordDialog(currentRecord: PasswordRecord): UserCommand = {
    println(s"Редактирование записи id=${currentRecord.id}")
    println("(оставьте поле пустым, чтобы сохранить прежнее значение)")

    val newService = readInput(s"Сервис [${currentRecord.service}]: ")
    val newUsername = readInput(s"Имя пользователя [${currentRecord.username}]: ")
    val newEmail = readInput(s"Почта [${currentRecord.email.getOrElse("-")}]: ")
    val newUrl = readInput(s"Адрес сайта [${currentRecord.url.getOrElse("-")}]: ")
    val newNote = readInput(s"Заметка [${currentRecord.note.getOrElse("-")}]: ")

    val changePassword = readConfirmation("Изменить пароль? [да/нет]: ")
    val newPassword = if (changePassword) {
      Some(readInput("Новый пароль: "))
    } else {
      None
    }

    EditRecord(
      id = currentRecord.id,
      service = if (newService.isEmpty) None else Some(newService),
      username = if (newUsername.isEmpty) None else Some(newUsername),
      password = newPassword,
      email = if (newEmail.isEmpty) None else Some(newEmail),
      url = if (newUrl.isEmpty) None else Some(newUrl),
      note = if (newNote.isEmpty) None else Some(newNote)
    )
  }

  def removeRecordDialog(record: PasswordRecord, confirmBeforeDelete: Boolean): UserCommand = {
    if (confirmBeforeDelete) {
      println("Будет удалена запись:")
      println(s"  Сервис: ${record.service}")
      println(s"  Имя пользователя: ${record.username}")
      val confirmed = readConfirmation("Удалить запись? [да/нет]: ")
      RemoveRecord(record.id, confirmed)
    } else {
      RemoveRecord(record.id, confirmed = true)
    }
  }

  def showError(error: AppError): String = {
    error.message
  }
}