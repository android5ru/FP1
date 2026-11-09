import java.io.File

object CommandProcessor {
  def process(state: AppState, config: AppConfig, input: String): CommandResult = {
    CommandParser.parse(input) match {
      case Left(error) =>
        Logger.warning(config.logFile, s"Ошибка парсинга команды: $error")
        CommandResult(state, error.message)

      case Right(command) =>
        val freshState = Commands.checkAutoLock(state, config)
        Commands.command(freshState, config, command)
    }
  }
}

object Commands {
  def checkAutoLock(state: AppState, config: AppConfig): AppState = state match {
    case u: Unlocked =>
      val elapsedMs = System.currentTimeMillis() - u.lastActivity
      val lockThresholdMs = config.automaticLockMinutes.toLong * 60 * 1000
      if (elapsedMs >= lockThresholdMs) {
        Logger.warning(config.logFile, s"Автоматическая блокировка: неактивность ${config.automaticLockMinutes} мин")
        Locked(0)
      } else u
    case other => other
  }

  def command(state: AppState, config: AppConfig, command: UserCommand): CommandResult = {
    command match {
      case ShowHelp => help(state, config)
      case Exit => exit(state, config)
      case CreateVault(pass, confirmPass) => create(state, config, pass, confirmPass)
      case OpenVault(pass) => open(state, config, pass)
      case LockVault => lock(state, config)
      case ReloadConfig => reload(state, config)
      case ListRecords => list(state, config)
      case ShowRecord(id) => show(state, config, id)
      case ShowPassword(id) => pass(state, config, id)
      case AddRecord(service, username, password, email, url, note) =>
        add(state, config, service, username, password, email, url, note)
      case EditRecord(id, service, username, password, email, url, note) =>
        edit(state, config, id, service, username, password, email, url, note)
      case RemoveRecord(id, confirmed) => remove(state, config, id, confirmed)
      case SearchRecords(query) => search(state, config, query)
    }
  }

  private def help(state: AppState, config: AppConfig): CommandResult = {
    val text =
      """Доступные команды:
        |  create      - Создать новое хранилище
        |  open        - Открыть существующее хранилище
        |  list        - Показать список записей
        |  show <id>   - Показать выбранную запись
        |  pass <id>   - Показать пароль выбранной записи
        |  add         - Добавить новую запись
        |  edit <id>   - Изменить существующую запись
        |  remove <id> - Удалить запись
        |  search <q>  - Найти записи по подстроке
        |  lock        - Заблокировать хранилище
        |  reload      - Перезагрузить конфигурацию
        |  exit        - Завершить работу""".stripMargin
    CommandResult(state, text)
  }

  def readInput(prompt: String): String = {
    print(prompt)
    scala.io.StdIn.readLine().trim
  }

  private def exit(state: AppState, config: AppConfig): CommandResult = {
    CommandResult(state, "Завершение работы...")
  }

  private def create(state: AppState, config: AppConfig, pass: String, confirmPass: String): CommandResult = {
    state match {
      case _: Unlocked => return CommandResult(state, VaultAlreadyOpen.message)
      case Blocked(r) => return CommandResult(state, AccessBlocked(r).message)
      case _ =>
    }

    if (pass.isEmpty) {
      return CommandResult(state, EmptyPassword.message)
    }
    if (pass != confirmPass) {
      return CommandResult(state, PasswordsDoNotMatch.message)
    }

    val key = Crypto.passwordToKey(pass)
    val emptyVault = Vault(List.empty)

    Storage.saveToFile(emptyVault, key, config.storageFile) match {
      case Right(_) =>
        Logger.info(config.logFile, "Новое хранилище успешно создано")
        CommandResult(state, "Хранилище успешно создано и сохранено.")
      case Left(err) =>
        Logger.error(config.logFile, s"Ошибка создания хранилища: ${err.message}")
        CommandResult(state, err.message)
    }
  }

  private def open(state: AppState, config: AppConfig, pass: String): CommandResult = {
    state match {
      case _: Unlocked => return CommandResult(state, VaultAlreadyOpen.message)
      case Blocked(r)  => return CommandResult(state, AccessBlocked(r).message)
      case l: Locked =>
        if (l.failedAttempts >= config.maximumUnlockAttempts) {
          return CommandResult(Blocked("Превышено максимальное количество попыток открытия."),
            AccessBlocked("Превышено максимальное количество попыток открытия.").message)
        }
      case _ =>
    }

    val file = new java.io.File(config.storageFile)
    if (!file.exists()) {
      return CommandResult(state, VaultNotFound(config.storageFile).message)
    }

    val key = Crypto.passwordToKey(pass)
    Storage.loadFromFile(config.storageFile, key) match {
      case Right(vault) =>
        Logger.info(config.logFile, "Хранилище успешно открыто")
        CommandResult(Unlocked(vault, key, System.currentTimeMillis()), "Хранилище успешно открыто.")

      case Left(err) =>
        val currentAttempts = state.asInstanceOf[Locked].failedAttempts
        val newAttempts = currentAttempts + 1

        if (newAttempts >= config.maximumUnlockAttempts) {
          Logger.error(config.logFile, "Превышено максимальное количество попыток открытия")
          CommandResult(Blocked("Превышено максимальное количество попыток открытия."),
            AccessBlocked("Превышено максимальное количество попыток открытия.").message)
        } else {
          val remaining = config.maximumUnlockAttempts - newAttempts
          CommandResult(Locked(newAttempts), WrongPassword(remaining).message)
        }
    }
  }

  private def lock(state: AppState, config: AppConfig): CommandResult = {
    state match {
      case _: Unlocked =>
        Logger.info(config.logFile, "Хранилище заблокировано пользователем")
        CommandResult(Locked(0), "Хранилище успешно заблокировано.")
      case _: Locked =>
        CommandResult(state, "Хранилище уже заблокировано.")
      case Blocked(r) =>
        CommandResult(state, s"Доступ заблокирован: $r")
    }
  }

  private def reload(state: AppState, config: AppConfig): CommandResult = {
    state match {
      case _: Unlocked =>
        return CommandResult(state, CannotReloadConfig.message)
      case _ =>
    }
    Config.load("config.json") match {
      case Right(newConfig) =>
        Logger.info(config.logFile, "Конфигурация успешно перезагружена")
        CommandResult(state, "Конфигурация успешно перезагружена.")
      case Left(errors) =>
        CommandResult(state, errors.message)
    }
  }

  private def list(state: AppState, config: AppConfig): CommandResult = {
    checkUnlocked(state).fold(
      err => CommandResult(state, err),
      u => {
        val sorted = config.defaultSort.toLowerCase match {
          case "id" => u.vault.records.sortBy(_.id)
          case "service" => u.vault.records.sortBy(_.service.toLowerCase)
          case "username" => u.vault.records.sortBy(_.username.toLowerCase)
          case "created" => u.vault.records.sortBy(_.createdAt)
          case "updated" => u.vault.records.sortBy(_.updatedAt)
          case _ => u.vault.records.sortBy(_.service.toLowerCase)
        }

        val output = if (sorted.isEmpty) VaultIsEmpty.message
        else AuxiliaryFunction.formatRecordsTable(sorted)

        Logger.info(config.logFile, s"Выполнен просмотр списка: записей=${sorted.size}")
        CommandResult(u.copy(lastActivity = System.currentTimeMillis()), output)
      }
    )
  }

  private def show(state: AppState, config: AppConfig, id: Int): CommandResult = {
    checkUnlocked(state).fold(
      err => CommandResult(state, err),
      u => AuxiliaryFunction.findRecordById(u.vault, id).fold(
        err => CommandResult(u, err.message),
        record => {
          Logger.info(config.logFile, s"Просмотрена запись: id=$id")
          CommandResult(u.copy(lastActivity = System.currentTimeMillis()), AuxiliaryFunction.formatRecordDetail(record))
        }
      )
    )
  }

  private def pass(state: AppState, config: AppConfig, id: Int): CommandResult = {
    checkUnlocked(state).fold(
      err => CommandResult(state, err),
      u => AuxiliaryFunction.findRecordById(u.vault, id).fold(
        err => CommandResult(u, err.message),
        record => {
          Logger.info(config.logFile, s"Отображён пароль записи: id=$id")
          CommandResult(u.copy(lastActivity = System.currentTimeMillis()),
            s"Пароль: ${record.password}")
        }
      )
    )
  }

  private def add(state: AppState, config: AppConfig, service: String, username: String,
                        password: String, email: Option[String], url: Option[String], note: Option[String]): CommandResult = {
    checkUnlocked(state) match {
      case Left(err) => CommandResult(state, err)
      case Right(u) =>
        if (service.isEmpty) return CommandResult(u, EmptyRequiredField("Сервис").message)
        if (username.isEmpty) return CommandResult(u, EmptyRequiredField("Имя пользователя").message)
        if (password.isEmpty) return CommandResult(u, EmptyRequiredField("Пароль").message)

        val newId = if (u.vault.records.isEmpty) 1 else u.vault.records.map(_.id).max + 1
        val now = java.time.LocalDateTime.now()
        val newRecord = PasswordRecord(newId, service, username, password, email, url, note, now, now)
        val newVault = u.vault.copy(records = u.vault.records :+ newRecord)

        Storage.saveToFile(newVault, u.masterKey, config.storageFile) match {
          case Right(_) =>
            Logger.info(config.logFile, s"Добавлена запись: id=$newId")
            CommandResult(u.copy(vault = newVault, lastActivity = System.currentTimeMillis()),
              s"Запись добавлена (id=$newId).")
          case Left(err) =>
            Logger.error(config.logFile, s"Ошибка сохранения: ${err.message}")
            CommandResult(u, err.message)
        }
    }
  }

  private def edit(state: AppState, config: AppConfig, id: Int, service: Option[String],
                         username: Option[String], password: Option[String], email: Option[String],
                         url: Option[String], note: Option[String]): CommandResult = {
    checkUnlocked(state).fold(
      err => CommandResult(state, err),
      u => AuxiliaryFunction.findRecordById(u.vault, id).fold(
        err => CommandResult(u, err.message),
        record => {
          val updatedRecord = record.copy(
            service  = service.getOrElse(record.service),
            username = username.getOrElse(record.username),
            password = password.filter(_.nonEmpty).getOrElse(record.password),
            email    = email,
            url      = url,
            note     = note,
            updatedAt = java.time.LocalDateTime.now()
          )

          val newVault = u.vault.copy(records = u.vault.records.map(r =>
            if (r.id == id) updatedRecord else r
          ))

          Storage.saveToFile(newVault, u.masterKey, config.storageFile).fold(
            err => CommandResult(u, err.message),
            _ => {
              Logger.info(config.logFile, s"Изменена запись: id=$id")
              CommandResult(u.copy(vault = newVault, lastActivity = System.currentTimeMillis()),
                s"Запись успешно изменена (id=$id).")
            }
          )
        }
      )
    )
  }

  private def remove(state: AppState, config: AppConfig, id: Int, confirmed: Boolean): CommandResult = {
    checkUnlocked(state).fold(
      err => CommandResult(state, err),
      u => AuxiliaryFunction.findRecordById(u.vault, id).fold(
        err => CommandResult(u, err.message),
        record => {
          if (!confirmed) {
            CommandResult(u, RecordDeletionCancelled.message)
          } else {
            val newVault = u.vault.copy(records = u.vault.records.filterNot(_.id == id))

            Storage.saveToFile(newVault, u.masterKey, config.storageFile).fold(
              err => {
                Logger.error(config.logFile, s"Ошибка при удалении: ${err.message}")
                CommandResult(u, err.message)
              },
              _ => {
                Logger.info(config.logFile, s"Удалена запись: id=$id")
                CommandResult(u.copy(vault = newVault, lastActivity = System.currentTimeMillis()),
                  s"Запись успешно удалена (id=$id).")
              }
            )
          }
        }
      )
    )
  }

  private def search(state: AppState, config: AppConfig, query: String): CommandResult = {
    checkUnlocked(state).fold(
      err => CommandResult(state, err),
      u => {
        val q = query.toLowerCase
        val found = u.vault.records.filter { r =>
          r.service.toLowerCase.contains(q) ||
            r.username.toLowerCase.contains(q) ||
            r.url.getOrElse("").toLowerCase.contains(q)
        }

        val output = if (found.isEmpty) SearchNotFound.message
        else s"Найдено записей: ${found.size}\n" + AuxiliaryFunction.formatRecordsTable(found)

        Logger.info(config.logFile, s"Выполнен поиск: найдено записей=${found.size}")
        CommandResult(u.copy(lastActivity = System.currentTimeMillis()), output)
      }
    )
  }

  private def checkUnlocked(state: AppState): Either[String, Unlocked] = state match {
    case u: Unlocked => Right(u)
    case Locked(_) => Left(VaultIsLocked.message)
    case Blocked(r) => Left(VaultIsBlocked(r).message)
  }
}