/*
 * Copyright 2022 mchalp.net
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.michalp.oneoftapirdemo

import sttp.tapir._
import sttp.tapir.json.circe._
import sttp.tapir.generic.auto._
import sttp.model.StatusCode
import io.circe.generic.semiauto._
import io.circe.Codec

object SimplifiedEndpoints {

  import model._

  val validateClient
      : Endpoint[String, String, Error, List[Order], Any] =
    endpoint.get
      .securityIn(auth.bearer[String]())
      .in("simple-validate")
      .in(path[String]("user"))
      .out(jsonBody[List[Order]])
      .errorOut(
        oneOf(notFoundStatus, unauthorized)
      )

  val validateServer
      : Endpoint[String, String, Error, List[Order], Any] =
    endpoint.get
      .securityIn(auth.bearer[String]())
      .in("simple-validate")
      .in(path[String]("user"))
      .out(jsonBody[List[Order]])
      .errorOut(
        oneOf(notFoundJson, unauthorized)
      )

  object model {

    val notFoundEmpty =
      oneOfVariantValueMatcher(
        StatusCode.NotFound,
        emptyOutputAs[Error](NotFound)
      ) { case NotFound => true }

    val notFoundStatus =
      oneOfVariantValueMatcher(
        statusCode(StatusCode.NotFound).map(_ => NotFound)(_ => ())
      ) { case NotFound => true }

    val notFoundJson =
      oneOfVariantValueMatcher(
        StatusCode.NotFound,
        jsonBody[Unit].map(_ => NotFound)(_ => ())
      ) { case NotFound => true }

    sealed trait Error extends Product with Serializable

    object Error {
      implicit def codec: Codec[Error] = deriveCodec
    }
    
    final case object NotFound extends Error {
      implicit val codec: Codec[NotFound.type] =
        deriveCodec
    }

    final case class Unauthorized(message: String) extends Error

    object Unauthorized {
      implicit val codec: Codec[Unauthorized] = deriveCodec
    }

    def unauthorized: EndpointOutput.OneOfVariant[Error] =
      oneOfVariantValueMatcher(
        StatusCode.Forbidden,
        jsonBody[Error].example(
          Unauthorized("You are not authorized")
        )
      ) { case _: Unauthorized => true }

  }

}
