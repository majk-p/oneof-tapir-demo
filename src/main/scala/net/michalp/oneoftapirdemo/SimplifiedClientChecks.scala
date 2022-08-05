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

class SimplifiedClientChecks[F[_]: MonadThrow](backend: SttpBackend[F, Any]) extends Checks[F] {

  private val validClient = SimplifiedClient.instance("secret", backend)
  private val invalidClient = SimplifiedClient.instance("invalid", backend)

  val verifyInvalidToken =
    invalidClient
      .getUserOrders("someuser")
      .attempt
      .map { result =>
        println(s"Result: $result")
        assert(result.isLeft)
        println("Successfully verified invalid token")
      }

  val verifyValidTokenInvalidUser =
    validClient
      .getUserOrders("someuser")
      .attempt
      .map { result =>
        println(s"Result: $result")
        assert(result.isRight)
        assert(result == Right(None))
        println("Successfully verified invalid user")
      }

  val verifyValidTokenValidUser =
    validClient
      .getUserOrders("test")
      .attempt
      .map { result =>
        println(s"Result: $result")
        assert(result.isRight)
        assert(result == Right(Some(List(Order("1"), Order("2")))))
        println("Successfully verified valid user")
      }

  val checklist = verifyInvalidToken *> verifyValidTokenInvalidUser *> verifyValidTokenValidUser
}

trait SimplifiedClient[F[_]] {
  def getUserOrders(user: String): F[Option[List[Order]]]
}

object SimplifiedClient {
  def instance[F[_]: MonadThrow](
      token: String,
      backend: SttpBackend[F, Any]
  ): SimplifiedClient[F] =
    new SimplifiedClient[F] {
      val sttpClient = SttpClientInterpreter()

      override def getUserOrders(user: String): F[Option[List[Order]]] = {
        val requestBuilder = sttpClient.toSecureRequest(
          SimplifiedEndpoints.validateClient,
          Some(uri"http://localhost:8080")
        )
        val request = requestBuilder(token)(user)
        val result = backend.send(request)
        result.map(_.body).flatMap {
          case Value(Left(v: SimplifiedEndpoints.model.Unauthorized)) =>
            MonadThrow[F].raiseError(new Exception(s"Error $v"))
          case Value(v) =>
            v.toOption.pure[F]
          case result =>
            MonadThrow[F].raiseError(new Exception(s"failed to decode response: $result"))
        }
      }

    }
}
