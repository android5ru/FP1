import scala.annotation.tailrec

object Main {
  @tailrec
  def loop(state: AppState, config: AppConfig): Unit = {
    val prompt = state match {
      case Locked(_) => "> "
      case _: Unlocked => "vault> "
      case Blocked(r) => s"BLOCKED ($r)> "
    }

    print(prompt)
    val input = scala.io.StdIn.readLine()

    if (input == null) {
      Logger.info(config.logFile, "Приложение завершено (конец ввода)")
      println("\nЗавершение работы...")
      return
    }

    CommandParser.parse(input) match {
      case Left(error) =>
        println(error.message)
        loop(state, config)

      case Right(command) =>
        val finalCommandOpt: Option[UserCommand] = command match {
          case CreateVault("", "") => AppUI.createVaultDialog(config.storageFile)
          case OpenVault("") =>
            val file = new java.io.File(config.storageFile)
            if (!file.exists()) {
              println(VaultNotFound(config.storageFile).message)
              None
            } else {
              AppUI.openVaultDialog()
            }
          case AddRecord("", "", "", None, None, None) =>
            state match {
              case _: Unlocked => AppUI.addRecordDialog()
              case Locked(_) =>
                println(VaultIsLocked.message)
                None
              case Blocked(r) =>
                println(AccessBlocked(r).message)
                None
            }
          case EditRecord(id, None, None, None, None, None, None) =>
            state match {
              case u: Unlocked =>
                u.vault.records.find(_.id == id) match {
                  case Some(record) => Some(AppUI.editRecordDialog(record))
                  case None =>
                    println(RecordNotFound(id).message)
                    None
                }
              case _ =>
                println(VaultIsLocked.message)
                None
            }

          case RemoveRecord(id, false) =>
            state match {
              case u: Unlocked =>
                u.vault.records.find(_.id == id) match {
                  case Some(record) => Some(AppUI.removeRecordDialog(record, config.confirmBeforeDelete))
                  case None =>
                    println(RecordNotFound(id).message)
                    None
                }
              case _ =>
                println(VaultIsLocked.message)
                None
            }
          case other => Some(other)
        }

        finalCommandOpt match {
          case Some(finalCommand) =>
            val freshState = Commands.checkAutoLock(state, config)
            val result = Commands.command(freshState, config, finalCommand)
            AppUI.showResult(result)
            result.logMessage.foreach(Logger.info(config.logFile, _))
            if (input.trim.toLowerCase != "exit") {
              loop(result.newState, config)
            } else {
              println("Работа завершена")
            }

          case None =>
            loop(state, config)
        }
    }
  }
  def main (args: Array[String]): Unit = {
    println("Локальное хранилище паролей\n")
    val configPath = "config.json"
    val tempLogPath = "vault.log"
    Logger.info(tempLogPath, "Приложение запущено")
    Config.load(configPath)match {
      case Right(config) =>
        Logger.info(config.logFile, "Конфигурация успешно загружена")
        println("Конфигурация успешно загружена")
        loop(Locked(0), config)
      case Left(errors) =>
        val errorMessage = errors.message
        println(errorMessage)
        Logger.error(tempLogPath, s"Ошибка конфигурации: ${errors.message}")
    }
  }
}