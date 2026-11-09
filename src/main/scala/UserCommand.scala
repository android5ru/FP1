sealed trait UserCommand

case class CreateVault(password: String, confirmPassword: String) extends UserCommand
case class OpenVault(password: String) extends UserCommand
case object LockVault extends UserCommand
case object ReloadConfig extends UserCommand

case object ListRecords extends UserCommand
case class ShowRecord(id: Int) extends UserCommand
case class ShowPassword(id: Int) extends UserCommand
case class AddRecord(
                      service: String,
                      username: String,
                      password: String,
                      email: Option[String],
                      url: Option[String],
                      note: Option[String]
                    ) extends UserCommand
case class EditRecord(
                       id: Int,
                       service: Option[String],
                       username: Option[String],
                       password: Option[String],
                       email: Option[String],
                       url: Option[String],
                       note: Option[String]
                     ) extends UserCommand
case class RemoveRecord(id: Int, confirmed: Boolean) extends UserCommand
case class SearchRecords(query: String) extends UserCommand

case object ShowHelp extends UserCommand
case object Exit extends UserCommand