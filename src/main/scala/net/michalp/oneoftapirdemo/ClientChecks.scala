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

import cats.MonadThrow
import cats.implicits._
import eu.timepit.refined.auto._
import sttp.client3._
import sttp.tapir.DecodeResult.Value
import sttp.tapir.client.sttp.SttpClientInterpreter
import sttp.tapir.client.sttp.WebSocketToPipe

import WebSocketToPipe.webSocketsNotSupported
import sttp.tapir.Endpoint
import OrderEndpoints.model.Error

class ClientChecks[F[_]: MonadThrow](backend: SttpBackend[F, Any], validationRoute: Endpoint[String, String, Error, Order, Any]) extends Checks[F] {

  private val validClient = Client.instance("secret", backend)(validationRoute)
  private val invalidClient = Client.instance("invalid", backend)(validationRoute)

  val verifyInvalidToken =
    invalidClient
      .getOrder("1")
      .attempt
      .map { result =>
        println(s"Result: $result")
        assert(result.isLeft)
        println("Successfully verified invalid token")
      }

  val verifyValidTokenInvalidUser =
    validClient
      .getOrder("999999")
      .attempt
      .map { result =>
        println(s"Result: $result")
        assert(result.isRight)
        assert(result == Right(None))
        println("Successfully verified invalid user")
      }

  val verifyValidTokenValidUser =
    validClient
      .getOrder("1")
      .attempt
      .map { result =>
        println(s"Result: $result")
        assert(result.isRight)
        assert(result == Right(Some(Order("1"))))
        println("Successfully verified valid user")
      }

  val checklist = verifyInvalidToken *> verifyValidTokenInvalidUser *> verifyValidTokenValidUser
}

trait Client[F[_]] {
  def getOrder(id: String): F[Option[Order]]
}

object Client {
  def instance[F[_]: MonadThrow](
    token: String,
    backend: SttpBackend[F, Any]
  )(
    validationRoute: Endpoint[String, String, Error, Order, Any]
  ): Client[F] =
    new Client[F] {
      val sttpClient = SttpClientInterpreter()

      override def getOrder(id: String): F[Option[Order]] = {
        val requestBuilder = sttpClient.toSecureRequest(
          validationRoute,
          Some(uri"http://localhost:8080")
        )
        val request = requestBuilder(token)(id)
        val result = backend.send(request)
        result.map(_.body).flatMap {
          case Value(Left(v: OrderEndpoints.model.Unauthorized)) =>
            MonadThrow[F].raiseError(new Exception(s"Error $v"))
          case Value(v) =>
            v.toOption.pure[F]
          case result =>
            MonadThrow[F].raiseError(new Exception(s"failed to decode response: $result"))
        }
      }

    }
}
