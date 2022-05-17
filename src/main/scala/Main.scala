import org.http4s.ember.client._
import org.http4s.client._
import org.http4s.client.dsl.io._
import org.http4s.{Uri, Request, Status, Header}
import org.http4s.headers._
import org.http4s.MediaType._
import org.http4s.Method._
import org.http4s.circe._
import cats.effect._
import cats.implicits._
import cats.data.EitherT
import io.circe._
import io.circe.literal._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.optics.JsonPath._
import java.util.concurrent._
import scala.concurrent.ExecutionContext
import org.http4s.EntityEncoder
import java.io.File
import cats.instances.duration
import org.http4s.syntax.header


object MubuExporter extends IOApp.Simple {

  def run: IO[Unit] = {

    val blockingEC = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(5))
    implicit val httpClient: Client[IO] = JavaNetClientBuilder[IO].create
    val target = Uri.uri("https://api2.mubu.com/v3/api/user/phone_login")

    val files = for {
      request <- EitherT.right(buildRequest(target))
      token <- getJWTtoken(request)
      fileList <- getAllFiles(token)
    } yield fileList

    files.value.map(println(_))
  }
  
  def buildRequest(url: Uri): IO[Request[IO]] = {
      
      val payload = for {
        _ <- IO.print("Enter your username: \n")
        name <- IO.readLine
        _ <- IO.print("Enter your password: \n")
        password <- IO.readLine
      } yield Payload(callbackType="0",phone=name,password=password)
      
      val request: IO[Request[IO]] = for {
        payloadJson <- payload.map(_.asJson)
      } yield POST(payloadJson, url)

      request
  }

  def getJWTtoken(request: Request[IO])(implicit client: Client[IO]): EitherT[IO, ErrorMsg, String] = {
    val login = client.run(request).use{
      case Status.Successful(res) => res.attemptAs[Json].leftMap(e => s"Error happened when decode").value
      case res => res.as[String].map(b => Left(s"Request $request failed with status ${res.status.code} and body $b"))
    }
    val _tokenPath = root.data.token.string

    val token = EitherT(login).leftMap(e => ErrorMsg(e)).map( j=>
        _tokenPath.getOption(j).getOrElse("No token"))
    token
  }

  def getAllFiles(token: String)(implicit client: Client[IO]): EitherT[IO, ErrorMsg, List[String]] = {
    val fileQuery = FileQuery("")
    val request = for {
      url <- Uri.fromString(RoutePath.GET_ALL_DOCUMENTS_PATH) 
    } yield POST(fileQuery.asJson, url).putHeaders(Header("jwt-token", token))

    val eitherTreq = EitherT(IO.pure(request)).leftMap(e => ErrorMsg(s"parse url ${RoutePath.GET_ALL_DOCUMENTS_PATH} error"))

    val response = eitherTreq.map(req => 
      client.run(req).use{
        case Status.Successful(res) => res.attemptAs[Json].leftMap(e => s"Decode File list error").value
        case res => res.as[String].map(e => Left(s"Get file list wrong response ${res.status.code} and body $e"))
    }).flatMap( res => {
      val _fileListPath = root.data.documents.each.id.string
      EitherT(res).leftMap(e => ErrorMsg(e)).map(value => {
         _fileListPath.getAll(value)
      })
    })

    response
  }
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
}

object RoutePath {
  val GET_SINGLE_DOC_PATH = "https://api2.mubu.com/v3/api/document/edit/get"
  val EXPORT_SINGLE_DOC_PATH = "https://mubu.com/convert/export"
  val PHONE_LOGIN_PATH = "https://api2.mubu.com/v3/api/user/phone_login"
  val GET_ALL_DOCUMENTS_PATH = "https://api2.mubu.com/v3/api/list/get_all_documents_page"
}



