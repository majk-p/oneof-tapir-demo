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
import sttp.tapir.client.sttp.SttpClientInterpreter
import sttp.tapir.client.sttp.WebSocketToPipe

import WebSocketToPipe.webSocketsNotSupported
import sttp.tapir.Endpoint
import OrderEndpoints.model._

class ClientChecks[F[_]: MonadThrow, E <: Error](backend: SttpBackend[F, Any], validationRoute: Endpoint[String, String, E, Order, Any]) extends Checks[F] {

  private val validClient = Client.instance("secret", backend)(validationRoute)
  private val invalidClient = Client.instance("invalid", backend)(validationRoute)

  val verifyInvalidToken =
    invalidClient
      .getOrder("1")
      .map { result =>
        println(s"Result: $result")
        require(result == Left(Unauthorized("Failed")))
        println("Successfully verified invalid token")
      }

  val verifyValidTokenInvalidOrder =
    validClient
      .getOrder("999999")
      .map { result =>
        println(s"Result: $result")
        require(result == Left(NotFound))
        println("Successfully verified invalid order")
      }

  val verifyValidTokenAndOrder =
    validClient
      .getOrder("1")
      .map { result =>
        println(s"Result: $result")
        require(result == Right(Order("1")))
        println("Successfully verified valid order")
      }

    val checklist = 
      (verifyValidTokenInvalidOrder *> verifyValidTokenAndOrder *> verifyInvalidToken)
        .handleError(error => println(s"${Console.RED}$error${Console.RESET}"))
}

trait Client[F[_]] {
  def getOrder(id: String): F[Either[Error, Order]]
}

object Client {
  def instance[F[_]: MonadThrow, E <: Error](
    token: String,
    backend: SttpBackend[F, Any]
  )(
    validationRoute: Endpoint[String, String, E, Order, Any]
  ): Client[F] =
    new Client[F] {
      val sttpClient = SttpClientInterpreter()

      override def getOrder(id: String): F[Either[Error, Order]] = {
        val requestBuilder = sttpClient.toSecureClientThrowDecodeFailures(
          validationRoute,
          Some(uri"http://localhost:8080"),
          backend
        )
        requestBuilder(token)(id).map(_.leftWiden)
      }

    }
}
