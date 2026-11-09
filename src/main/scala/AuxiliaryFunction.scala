import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object AuxiliaryFunction {
  val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
  def formatDate(dt: LocalDateTime): String = dt.format(dateFormatter)

  def findRecordById(vault: Vault, id: Int): Either[RecordError, PasswordRecord] = {
    vault.records.find(_.id == id).toRight(RecordNotFound(id))
  }
  def formatRecordsTable(records: List[PasswordRecord]): String = {
    if (records.isEmpty) return "Записи не найдены."
    val headers = List("ID", "Сервис", "Имя пользователя", "Почта", "Адрес", "Изменена")
    val rows = records.map { r =>
      List(
        r.id.toString,
        r.service,
        r.username,
        r.email.getOrElse("-"),
        r.url.getOrElse("-"),
        formatDate(r.updatedAt)
      )
    }
    val allRows = headers :: rows
    val widths = headers.indices.map { i =>
      allRows.map(row => row(i).length).max
    }
    def pad(s: String, width: Int): String = s + " " * (width - s.length)
    val headerLine = headers.zip(widths).map { case (h, w) => pad(h, w) }.mkString("  ")
    val separator = widths.map("-" * _).mkString("  ")
    val dataLines = rows.map(row => row.zip(widths).map { case (c, w) => pad(c, w) }.mkString("  "))

    (headerLine :: separator :: dataLines).mkString("\n")
  }

  def formatRecordDetail(record: PasswordRecord): String = {
    s"""Сервис: ${record.service}
       |Адрес: ${record.url.getOrElse("-")}
       |Имя пользователя: ${record.username}
       |Почта: ${record.email.getOrElse("-")}
       |Пароль: ********
       |Заметка: ${record.note.getOrElse("-")}
       |Создана: ${formatDate(record.createdAt)}
       |Изменена: ${formatDate(record.updatedAt)}""".stripMargin
  }
}
