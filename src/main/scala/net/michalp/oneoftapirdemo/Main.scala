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

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      client <- AsyncHttpClientCatsBackend[IO]()
      clientChecks = new ClientChecks[IO](client)
      simplifiedClientChecks = new SimplifiedClientChecks[IO](client)
      result <- Server.resource
        .use { _ =>
          (
            Checks.checklist, 
            clientChecks.checklist,
            simplifiedClientChecks.checklist
          ).parTupled.void
        }
        .as(ExitCode.Success)
    } yield result 


  }
}
