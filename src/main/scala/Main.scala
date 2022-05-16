import org.http4s.ember.client._
import org.http4s.client._
import org.http4s.client.dsl.io._
import org.http4s.Uri
import org.http4s.Request
import org.http4s.headers._
import org.http4s.MediaType._
import org.http4s.Method._
import org.http4s.circe._
import cats.effect._
import io.circe._
import io.circe.literal._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.optics.JsonPath._
import java.util.concurrent._
import scala.concurrent.ExecutionContext
import org.http4s.EntityEncoder


object MubuExporter extends IOApp.Simple {

  def run: IO[Unit] = {

    val blockingEC = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(5))
    implicit val httpClient: Client[IO] = JavaNetClientBuilder[IO].create
    val target = Uri.uri("https://api2.mubu.com/v3/api/user/phone_login")

    for{
      request <- buildRequest(target)
      token <- getJWTtoken(request)
    } yield IO{println(token)}
    
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

  def getJWTtoken(request: Request[IO])(implicit client: Client[IO]): IO[String] = {
    val login = client.expect[Json](request)
    val _tokenPath = root.data.token.string
    val token = login.map(json => _tokenPath.getOption(json).getOrElse("No token"))
    token
  }

  case class Payload(
    callbackType: String,
    phone: String,
    password: String
  )

  // case class AuthResponse(
  //   code: String,
  //   data: DataResponse
  // )
  
  // case class DataResponse(
  //   googleId: String,
  //   gender: String,
  //   year: String,
  //   city: String,
  //   remark: String,
  //   view: String,
  //   province: String,
  //   googleName: String,
  //   id: String,
  //   wxName: String,
  //   email: String,
  //   vipEndDate: String,
  //   clientId: String,
  //   level: String,
  //   encryptPassword: String,
  //   facebookId: String,
  //   photo: String,
  //   updateTime: String,
  //   wxId: String,
  //   passSecure: String,
  //   sort: String,
  //   appleId: String,
  //   qqId: String,
  //   appleName: String,
  //   agreeTermService: String,
  //   token: String,
  //   phone: String,
  //   createTime: String,
  //   name: String,
  //   anonymUserFlag: String,
  //   qqName: String,
  //   toutiaoId: String,
  //   facebookName: String,
  //   larkId: String,
  // )
}



