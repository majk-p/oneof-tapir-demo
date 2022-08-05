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

import cats.effect._
import cats.implicits._
import OrderEndpoints.model._

class OrderRoutes[F[_]: Sync] {

  private val onlyValidToken = "secret"

  def securityLogic(token: String): F[Either[Failure[NotFound.type], Unit]] = 
    Either.cond(token == onlyValidToken, (), Unauthorized("Failed") : Failure[NotFound.type]).pure[F]


  private val userOrders: Map[String, List[Order]] = Map (
    "test" -> List(Order("1"), Order("2"))
  )

  def businessLogic(user: String): F[Either[Failure[NotFound.type], List[Order]]] = 
    Either
      .fromOption(userOrders.get(user), BusinessFailure(NotFound))
      .leftWiden[Failure[NotFound.type]]
      .pure[F]

  val validate = 
    OrderEndpoints
      .validateServer.serverSecurityLogic(securityLogic(_))
      .serverLogic(_ => user => businessLogic(user))
}