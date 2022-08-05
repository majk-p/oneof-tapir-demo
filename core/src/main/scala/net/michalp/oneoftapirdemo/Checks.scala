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
import eu.timepit.refined.auto._
import sttp.client3._

object Checks {
  private val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()

  private val baseUri = uri"http://localhost:8080/validate"
  private val request = basicRequest
    .response(asStringAlways)

  val verifyInvalidToken = IO {
    val result = 
      request
        .auth
        .bearer("invalid")
        .get(baseUri.addPath("someuser"))
        .send(backend)
    println(s"Response = ${result.body} Code = ${result.code}")
    assert(result.code.code == 403)
    println("Successfully verified invalid token")
  }

  val verifyValidTokenInvalidUser = IO {
    val result = 
      request
        .auth
        .bearer("secret")
        .get(baseUri.addPath("someuser"))
        .send(backend)
    println(s"Response = ${result.body} Code = ${result.code}")
    assert(result.code.code == 404)
    println("Successfully verified invalid user")
  }

  val verifyValidTokenValidUser = IO {
    val result = 
      request
        .auth
        .bearer("secret")
        .get(baseUri.addPath("test"))
        .send(backend)
    println(s"Response = ${result.body} Code = ${result.code}")
    assert(result.code.code == 200)
    println("Successfully verified valid user")
  }

}
