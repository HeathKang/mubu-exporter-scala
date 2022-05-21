import org.http4s.ember.client._
import org.http4s.client._
import org.http4s.client.dsl.io._
import org.http4s.{Uri, Request, Status, Header}
import org.http4s.headers._
import org.http4s.MediaType._
import org.http4s.Method._
import org.http4s.circe._
import cats.effect._
import cats.effect.std._
import cats.effect.implicits._
import cats.implicits._
import cats._
import cats.data.EitherT
import io.circe._
import io.circe.literal._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.optics.JsonPath._
import java.util.concurrent._
import scala.concurrent.ExecutionContext
import org.http4s.EntityEncoder
import cats.instances.duration
import org.http4s.syntax.header
import scala.io.StdIn.readLine
import cats.data.EitherTMonad

object MubuExporter extends IOApp.Simple {
  import Model._
  import Mubu._
  import RoutePath._

  def run: IO[Unit] = {

    val blockingEC =
      ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(5))
    val httpClient: Client[IO] = JavaNetClientBuilder[IO].create

    val mubuClient = Mubu.MubuInterpreter.make[IO](httpClient)
    val reqBuilder = Mubu.RequestBuilderInterpreter.make[IO]

    val filesListContent = for {
      getJwtReq <- EitherT.right(reqBuilder.buildJWTRequest)
      token <- EitherT(mubuClient.getJWTtoken(getJwtReq))
      getAllFilesListReq <- EitherT.right(
        reqBuilder.buildGetAllFilesListRequest(token)
      )
      filesList <- EitherT(mubuClient.getAllFilesList(getAllFilesListReq))
      filesListReq <- filesList
        .parTraverse(file =>
          EitherT(
            reqBuilder
              .buildGetSingleDocContentRequest(token, file)
              .map(Either.right[ErrorMsg, Request[IO]](_))
          )
        )
      fileListContent <- filesListReq
        .parTraverseN(2)(fileReq =>
          EitherT(mubuClient.getSingleDocContent(fileReq))
        )

      filesListExportReq <- fileListContent.parTraverse(fileSingleContent =>
        EitherT(
          reqBuilder
            .buildExportSingleDocContentRequest(token, fileSingleContent)
            .map(Either.right[ErrorMsg, Request[IO]](_))
        )
      )

      // filesListExport <- filesListExportReq.parTraverse(
      //   fileSingleReq =>
      //     EitherT(mubuClient.exportSingleDocContent(fileSingleReq))
      // )
      
    } yield filesListExportReq

    // val files = filesListContent.flatMap(
    //     _
    //     .map(fileReq => EitherT[IO,ErrorMsg,String](mubuClient.getSingleDocContent(fileReq)))
    //     .sequence
    // )
    filesListContent.fold(println(_), println(_))

  }

  def buildRequest(url: Uri): IO[Request[IO]] = {

    val payload = for {
      _ <- IO.print("Enter your username: \n")
      name <- IO.readLine
      _ <- IO.print("Enter your password: \n")
      password <- IO.readLine
    } yield Payload(callbackType = "0", phone = name, password = password)

    val request: IO[Request[IO]] = for {
      payloadJson <- payload.map(_.asJson)
    } yield POST(payloadJson, url)

    request
  }

  def getJWTtoken(
      request: Request[IO]
  )(implicit client: Client[IO]): EitherT[IO, ErrorMsg, String] = {
    val login = client.run(request).use {
      case Status.Successful(res) =>
        res.attemptAs[Json].leftMap(e => s"Error happened when decode").value
      case res =>
        res
          .as[String]
          .map(b =>
            Left(
              s"Request $request failed with status ${res.status.code} and body $b"
            )
          )
    }
    val _tokenPath = root.data.token.string

    val token = EitherT(login)
      .leftMap(e => ErrorMsg(e))
      .map(j => _tokenPath.getOption(j).getOrElse("No token"))
    token
  }

  def getAllFiles(
      token: String
  )(implicit client: Client[IO]): EitherT[IO, ErrorMsg, List[String]] = {
    val fileQuery = FileQuery("")
    val request = for {
      url <- Uri.fromString(RoutePath.GET_ALL_DOCUMENTS_PATH)
    } yield POST(fileQuery.asJson, url).putHeaders(Header("jwt-token", token))

    val eitherTreq = EitherT(IO.pure(request)).leftMap(e =>
      ErrorMsg(s"parse url ${RoutePath.GET_ALL_DOCUMENTS_PATH} error")
    )

    val response = eitherTreq
      .map(req =>
        client.run(req).use {
          case Status.Successful(res) =>
            res.attemptAs[Json].leftMap(e => s"Decode File list error").value
          case res =>
            res
              .as[String]
              .map(e =>
                Left(
                  s"Get file list wrong response ${res.status.code} and body $e"
                )
              )
        }
      )
      .flatMap(res => {
        val _fileListPath = root.data.documents.each.id.string
        EitherT(res)
          .leftMap(e => ErrorMsg(e))
          .map(value => {
            _fileListPath.getAll(value)
          })
      })

    response
  }

  // def getSingleDoc(path: String)(implicit client: Client[IO])

}

object RoutePath {
  val GET_SINGLE_DOC_PATH = "https://api2.mubu.com/v3/api/document/edit/get"
  val EXPORT_SINGLE_DOC_PATH = "https://mubu.com/convert/export"
  val PHONE_LOGIN_PATH = "https://api2.mubu.com/v3/api/user/phone_login"
  val GET_ALL_DOCUMENTS_PATH =
    "https://api2.mubu.com/v3/api/list/get_all_documents_page"

}

object Mubu {
  import Model._
  import RoutePath._

  type EitherMsg[A] = Either[ErrorMsg, A]
  trait Mubu[F[_]] {
    def getJWTtoken(request: Request[F]): F[EitherMsg[String]]
    def getAllFilesList(request: Request[F]): F[EitherMsg[List[String]]]
    def getSingleDocContent(request: Request[F]): F[EitherMsg[String]]
    def exportSingleDocContent(request: Request[F]): F[EitherMsg[Array[Byte]]]
  }

  trait RequestBuilder[F[_]] {
    def buildJWTRequest: F[Request[F]]
    def buildGetAllFilesListRequest(token: String): F[Request[F]]
    def buildGetSingleDocContentRequest(
        token: String,
        fileId: String
    ): F[Request[F]]
    def buildExportSingleDocContentRequest(
        token: String,
        content: String
    ): F[Request[F]]
  }

  object RequestBuilderInterpreter {
    def make[F[_]: Async]: RequestBuilder[F] = {
      new RequestBuilderInterpreter[F]()
    }
  }
  class RequestBuilderInterpreter[F[_]: Sync] extends RequestBuilder[F] {
    override def buildJWTRequest: F[Request[F]] = {

      val uri = Uri.unsafeFromString(RoutePath.PHONE_LOGIN_PATH)
      val request: F[Request[F]] = for {
        _ <- Sync[F].delay(println("Enter your username: \n"))
        name <- Sync[F].delay(readLine)
        _ <- Sync[F].delay(println("Enter your password: \n"))
        password <- Sync[F].delay(readLine)
        payload <- Payload(
          callbackType = "0",
          phone = name,
          password = password
        ).pure(Sync[F])
        payloadJson <- payload.asJson.pure(Sync[F])
      } yield Request[F](POST, uri).withEntity[Json](payloadJson)

      request
    }
    override def buildGetAllFilesListRequest(token: String): F[Request[F]] = {

      val uri = Uri.unsafeFromString(RoutePath.GET_ALL_DOCUMENTS_PATH)
      val request: F[Request[F]] = for {
        fileQuery <- FileQuery("").asJson.pure(Sync[F])
        header <- Header("jwt-token", token).pure(Sync[F])
      } yield Request[F](POST, uri)
        .withHeaders(header)
        .withEntity[Json](fileQuery)

      request
    }
    override def buildGetSingleDocContentRequest(
        token: String,
        fileId: String
    ): F[Request[F]] = {

      val uri = Uri.unsafeFromString(RoutePath.GET_SINGLE_DOC_PATH)
      val request = for {
        singleDocQuery <- SingleDocQuery(fileId).asJson.pure(Sync[F])
        header <- Header("jwt-token", token).pure(Sync[F])
      } yield Request[F](POST, uri)
        .withHeaders(header)
        .withEntity[Json](singleDocQuery)

      request
    }

    override def buildExportSingleDocContentRequest(
        token: String,
        content: String
    ): F[Request[F]] = {

      val uri = Uri.unsafeFromString(RoutePath.EXPORT_SINGLE_DOC_PATH)
      val request = for {
        singleDocContent <- SingleDocContent(definition = content).asJson.pure(Sync[F])
        header <- Header("jwt-token", token).pure(Sync[F])
      } yield Request[F](POST, uri)
        .withHeaders(header)
        .withEntity[Json](singleDocContent)

      request
    }
  }

  object MubuInterpreter {
    def make[F[_]: Async](client: Client[F]): MubuInterpreter[F] = {
      new MubuInterpreter(client)
    }
  }
  class MubuInterpreter[F[_]: Async] private (client: Client[F])
      extends Mubu[F] {
    override def getJWTtoken(request: Request[F]): F[EitherMsg[String]] = {
      val login: F[Either[String, Json]] = client.run(request).use {
        case Status.Successful(res) =>
          res
            .attemptAs[Json]
            .leftMap(e => s"Decode jwt token content to json error")
            .value
        case res =>
          res
            .as[String]
            .map(b =>
              Left(
                s"Request $request failed with status ${res.status.code} and body $b"
              )
            )
      }
      val _tokenPath = root.data.token.string

      val token = EitherT(login)
        .leftMap(e => ErrorMsg(e))
        .map(j => _tokenPath.getOption(j).getOrElse("No token"))

      token.value
    }

    override def getAllFilesList(
        request: Request[F]
    ): F[EitherMsg[List[String]]] = {
      val res: F[Either[String, Json]] = client.run(request).use {
        case Status.Successful(res) =>
          res
            .attemptAs[Json]
            .leftMap(e => s"Decode File list to json error")
            .value
        case res =>
          res
            .as[String]
            .map(b =>
              Left(
                s"Request $request failed with status ${res.status.code} and body $b"
              )
            )
      }
      val _fileListPath = root.data.documents.each.id.string
      val result = EitherT(res)
        .leftMap(e => ErrorMsg(e))
        .map(value => {
          _fileListPath.getAll(value)
        })
        .value

      result
    }

    override def getSingleDocContent(
        request: Request[F]
    ): F[EitherMsg[String]] = {
      val res: F[Either[String, Json]] = client.run(request).use {
        case Status.Successful(res) =>
          res
            .attemptAs[Json]
            .leftMap(e => s"Decode Single doc to json error")
            .value
        case res =>
          res
            .as[String]
            .map(b =>
              Left(
                s"Request $request failed with status ${res.status.code} and body $b"
              )
            )
      }

      val _doc_content_path = root.data.definition.string

      val result = EitherT(res)
        .leftMap(e => ErrorMsg(e))
        .flatMap(value => {
          EitherT.fromEither[F](
            _doc_content_path
              .getOption(value)
              .fold[Either[ErrorMsg, String]](
                Left(ErrorMsg(s"can't get single doc content ${value}"))
              )(Right(_))
          )
        })
        .value

      result
    }

    override def exportSingleDocContent(
        request: Request[F]
    ): F[EitherMsg[Array[Byte]]] = {
      val res: F[EitherMsg[Array[Byte]]] = client.run(request).use {
        case Status.Successful(res) =>
          res
            .attemptAs[Array[Byte]]
            .leftMap(e => ErrorMsg(s"Decode Single doc to json error"))
            .value
        case res =>
          res
            .as[String]
            .map(b =>
              Left(
                ErrorMsg(
                  s"Request $request failed with status ${res.status.code} and body $b"
                )
              )
            )
      }

      res
    }
  }

}

object Model {
  case class Payload(
      callbackType: String,
      phone: String,
      password: String
  )

  case class FileQuery(
      start: String = ""
  )

  case class ErrorMsg(
      msg: String
  )

  case class SingleDocQuery(
      docId: String,
      password: String = ""
  )

  case class SingleDocContent(
      definition: String,
      `type`: String = "pdf"
  )
}
