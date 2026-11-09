import java.io._
import java.nio.file.{Files, Paths}
import scala.util.Try

object Storage {
  def serializeVault(vault: Vault): Either[StorageError, Array[Byte]] = {
    Try {
      val bos = new ByteArrayOutputStream()
      val oos = new ObjectOutputStream(bos)
      try {
        oos.writeObject(vault)
        oos.flush()
        bos.toByteArray
      } finally oos.close()
    }.toEither.left.map(ex => StorageWriteError(ex.getMessage))
  }
  def deserializeVault(bytes: Array[Byte]): Either[StorageError, Vault] = {
    Try {
      val bis = new ByteArrayInputStream(bytes)
      val ois = new ObjectInputStream(bis)
      try {
        ois.readObject().asInstanceOf[Vault]
      } finally ois.close()
    }.toEither.left.map(ex => StorageReadError(ex.getMessage))
  }
  def saveToFile(vault: Vault, key: Array[Byte], path: String): Either[StorageError, Unit] = {
    for {
      rawBytes <- serializeVault(vault)
      encryptedBytes <- Crypto.encrypt(rawBytes, key)
      _ <- writeBytesToFile(encryptedBytes, path)
    } yield ()
  }

  def loadFromFile(path: String, key: Array[Byte]): Either[StorageError, Vault] = {
    for {
      fileBytes <- readBytesFromFile(path)
      decryptedBytes <- Crypto.decrypt(fileBytes, key)
      vault <- deserializeVault(decryptedBytes)
    } yield vault
  }
  def writeBytesToFile(bytes: Array[Byte], path: String): Either[StorageError, Unit] = {
    Try {
      Files.write(Paths.get(path), bytes)
      ()
    }.toEither.left.map(ex => StorageWriteError(ex.getMessage))
  }
  def readBytesFromFile(path: String): Either[StorageError, Array[Byte]] = {
    Try {
      Files.readAllBytes(Paths.get(path))
    }.toEither.left.map(ex => StorageReadError(ex.getMessage))
  }
}
