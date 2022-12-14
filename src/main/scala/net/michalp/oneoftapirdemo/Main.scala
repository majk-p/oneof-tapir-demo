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

import cats.effect.IO
import cats.effect.IOApp
import cats.effect._
import cats.implicits._
import sttp.client3.asynchttpclient.cats.AsyncHttpClientCatsBackend
import sttp.client3.SttpBackend
import net.michalp.oneoftapirdemo.OrderEndpoints.model.Error

object Main extends IOApp {

  private val separator = IO.println("="*20)

  private val basicTest: IO[Unit] = separator *> IO.println("basic checks") *> Checks.checklist.void

  private def testWhereClientAndServerUseTheSameEndpointDefinition(backend: SttpBackend[IO, Any]) = 
    separator *>
      IO.println("client checks where client uses server endpoint definition") *>
      new ClientChecks[IO, Error](backend, OrderEndpoints.Server.findOrder).checklist

  private def testWhereClientUsesSimplifiedDefinition(backend: SttpBackend[IO, Any]) = 
    separator *>
      IO.println("client checks where client uses client specific endpoint definition") *>
      new ClientChecks[IO, Error](backend, OrderEndpoints.Client.findOrder).checklist


  private def testWhereClientUsesEvenMoreSimplifiedDefinition(backend: SttpBackend[IO, Any]) = 
    separator *>
      IO.println("client checks where client uses client specific endpoint definition with only not found supported") *>
      new ClientChecks[IO, OrderEndpoints.model.NotFound.type](backend, OrderEndpoints.Client.findOrderWithSingleError).checklist
  
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      client <- AsyncHttpClientCatsBackend[IO]()
      result <- Server.resource
        .use { _ =>
          basicTest *> 
          testWhereClientAndServerUseTheSameEndpointDefinition(client) *> 
          testWhereClientUsesSimplifiedDefinition(client) *>
          testWhereClientUsesEvenMoreSimplifiedDefinition(client)
        }
        .as(ExitCode.Success)
    } yield result 


  }
}