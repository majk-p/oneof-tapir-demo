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

object OrderEndpoints {

  import model._

  val validateClient
      : Endpoint[String, String, Failure[NotFound.type], List[Order], Any] =
    endpoint.get
      .securityIn(auth.bearer[String]())
      .in("validate")
      .in(path[String]("user"))
      .out(jsonBody[List[Order]])
      .errorOut(
        oneOf(notFoundStatus, unauthorized)
      )

  val validateServer
      : Endpoint[String, String, Failure[NotFound.type], List[Order], Any] =
    endpoint.get
      .securityIn(auth.bearer[String]())
      .in("validate")
      .in(path[String]("user"))
      .out(jsonBody[List[Order]])
      .errorOut(
        oneOf(notFoundJson, unauthorized)
      )

  object model {

    val notFoundEmpty =
      oneOfVariantValueMatcher(
        StatusCode.NotFound,
        emptyOutputAs[Failure[NotFound.type]](BusinessFailure(NotFound))
      ) { case BusinessFailure(NotFound) => true }

    val notFoundStatus =
      oneOfVariantValueMatcher(
        statusCode(StatusCode.NotFound).map(_ => BusinessFailure(NotFound))(_ => ())
      ) { case BusinessFailure(NotFound) => true }

    val notFoundJson =
      oneOfVariantValueMatcher(
        StatusCode.NotFound,
        jsonBody[Unit].map(_ => BusinessFailure(NotFound))(_ => ())
      ) { case BusinessFailure(NotFound) => true }

    final case object NotFound {
      implicit val codec: io.circe.Codec[NotFound.type] =
        deriveCodec[NotFound.type]
    }

    sealed trait Failure[+E] extends Product with Serializable

    object Failure {
      implicit def codec[E: Codec]: Codec[Failure[E]] = deriveCodec
    }

    sealed trait SecurityFailure extends Failure[Nothing]
    object SecurityFailure {
      implicit def codec: Codec[SecurityFailure] = deriveCodec
    }

    final case class Unauthorized(message: String) extends SecurityFailure

    object Unauthorized {
      implicit val codec: Codec[Unauthorized] = deriveCodec
    }

    def unauthorized: EndpointOutput.OneOfVariant[SecurityFailure] =
      oneOfVariantValueMatcher(
        StatusCode.Forbidden,
        jsonBody[SecurityFailure].example(
          Unauthorized("You are not authorized")
        )
      ) { case _: Unauthorized => true }

    final case class BusinessFailure[+E](source: E) extends Failure[E]

    object BusinessFailure {
      implicit def codec[E: Codec]: Codec[BusinessFailure[E]] = deriveCodec
    }

  }

}
