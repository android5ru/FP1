sealed trait AppError {
  def message: String
}

//Ошибки парсинга команд
sealed trait ParseError extends AppError

case class UnknownCommand(cmdName: String) extends ParseError {
  def message: String = s"Неизвестная команда: '$cmdName'. Введите 'help' для списка команд."
}

case object EmptyCommand extends ParseError {
  def message: String = "Пустая команда. Введите 'help' для списка команд."
}

case class InvalidIdFormat(value: String) extends ParseError {
  def message: String = s"Неверный формат ID: '$value'. Ожидается целое число."
}

case class MissingIdArgument(command: String) extends ParseError {
  def message: String = s"Не указан ID записи. Использование: $command <id>"
}

case object MissingSearchQuery extends ParseError {
  def message: String = "Не указан поисковый запрос. Использование: search <запрос>"
}

//Ошибки валидации данных
sealed trait ValidationError extends AppError

case class EmptyRequiredField(field: String) extends ValidationError {
  def message: String = s"Ошибка: поле '$field' обязательно для заполнения."
}

case object PasswordsDoNotMatch extends ValidationError {
  def message: String = "Ошибка: Мастер-пароли не совпадают."
}

case object EmptyPassword extends ValidationError {
  def message: String = "Ошибка: Пароль не может быть пустым."
}

//Ошибки работы с хранилищем
sealed trait StorageError extends AppError

case class VaultNotFound(path: String) extends StorageError {
  def message: String = s"Ошибка: Файл хранилища '$path' не найден. Создайте новое хранилище (create)."
}

case class VaultCorrupted(reason: String) extends StorageError {
  def message: String = s"Ошибка: Хранилище повреждено. $reason"
}

case class StorageWriteError(reason: String) extends StorageError {
  def message: String = s"Ошибка: Не удалось сохранить хранилище. $reason"
}

case class StorageReadError(reason: String) extends StorageError {
  def message: String = s"Ошибка: Не удалось прочитать хранилище. $reason"
}

//Ошибки аутентификации
sealed trait AuthError extends AppError

case class WrongPassword(attemptsLeft: Int) extends AuthError {
  def message: String = s"Ошибка: Неверный мастер-пароль или файл поврежден. Осталось попыток: $attemptsLeft"
}

case class AccessBlocked(reason: String) extends AuthError {
  def message: String = s"Ошибка: $reason"
}

case object VaultAlreadyOpen extends AuthError {
  def message: String = "Ошибка: Хранилище уже открыто."
}

//Ошибки состояния
sealed trait StateError extends AppError

case object VaultIsLocked extends StateError {
  def message: String = "Ошибка: Хранилище заблокировано. Введите 'open' для открытия."
}

case class VaultIsBlocked(reason: String) extends StateError {
  def message: String = s"Ошибка: Доступ заблокирован. $reason"
}

case object CannotReloadConfig extends StateError {
  def message: String = "Ошибка: невозможно перезагрузить конфигурацию при открытом хранилище. Сначала заблокируйте хранилище (lock)."
}

//Ошибки работы с записями
sealed trait RecordError extends AppError

case class RecordNotFound(id: Int) extends RecordError {
  def message: String = s"Ошибка: запись с id=$id не существует."
}

case object RecordDeletionCancelled extends RecordError {
  def message: String = "Удаление отменено пользователем."
}

case object VaultIsEmpty extends RecordError {
  def message: String = "Хранилище пусто. Добавьте первую запись командой 'add'."
}

case object SearchNotFound extends RecordError {
  def message: String = "Ничего не найдено."
}

//Ошибки конфигурации
sealed trait ConfigError extends AppError

case class ConfigFileNotFound(path: String) extends ConfigError {
  def message: String = s"Не удалось прочитать файл '$path': файл не найден"
}

case class ConfigParseError(reason: String) extends ConfigError {
  def message: String = s"Ошибка формата JSON: $reason"
}

case class ConfigValidationError(errors: List[String]) extends ConfigError {
  def message: String = "Ошибка конфигурации:\n" +
    errors.zipWithIndex.map { case (err, i) => s"  ${i + 1}. $err" }.mkString("\n")
}
