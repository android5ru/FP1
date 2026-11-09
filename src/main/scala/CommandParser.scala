import scala.util.Try

object CommandParser {
  def parse(input: String): Either[ParseError, UserCommand] = {
    val trimmed = input.trim
    if (trimmed.isEmpty) {
      return Left(EmptyCommand)
    }

    val parts = trimmed.split("\\s+").toList
    val cmdName = parts.head.toLowerCase
    val args = parts.tail

    cmdName match {
      case "create" => Right(CreateVault("", ""))
      case "open" => Right(OpenVault(""))
      case "list" => Right(ListRecords)
      case "show" => parseShow(args)
      case "pass" => parsePass(args)
      case "add" => Right(AddRecord("", "", "", None, None, None))
      case "edit" => parseEdit(args)
      case "remove" => parseRemove(args)
      case "search" => parseSearch(args)
      case "lock" => Right(LockVault)
      case "reload" => Right(ReloadConfig)
      case "help" => Right(ShowHelp)
      case "exit" => Right(Exit)
      case _ => Left(UnknownCommand(cmdName))
    }
  }

  private def parseShow(args: List[String]): Either[ParseError, UserCommand] = {
    for {
      idStr <- args.headOption.toRight(MissingIdArgument("show"))
      id <- Try(idStr.toInt).toOption.toRight(InvalidIdFormat(idStr))
    } yield ShowRecord(id)
  }

  private def parsePass(args: List[String]): Either[ParseError, UserCommand] = {
    for {
      idStr <- args.headOption.toRight(MissingIdArgument("pass"))
      id <- Try(idStr.toInt).toOption.toRight(InvalidIdFormat(idStr))
    } yield ShowPassword(id)
  }

  private def parseEdit(args: List[String]): Either[ParseError, UserCommand] = {
    for {
      idStr <- args.headOption.toRight(MissingIdArgument("edit"))
      id <- Try(idStr.toInt).toOption.toRight(InvalidIdFormat(idStr))
    } yield EditRecord(id, None, None, None, None, None, None)
  }

  private def parseRemove(args: List[String]): Either[ParseError, UserCommand] = {
    for {
      idStr <- args.headOption.toRight(MissingIdArgument("remove"))
      id <- Try(idStr.toInt).toOption.toRight(InvalidIdFormat(idStr))
    } yield RemoveRecord(id, confirmed = false)
  }

  private def parseSearch(args: List[String]): Either[ParseError, UserCommand] = {
    args.headOption
      .map(_ => SearchRecords(args.mkString(" ")))
      .toRight(MissingSearchQuery)
  }
}