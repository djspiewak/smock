/*
 * Copyright 2017 Daniel Spiewak
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

package smock

import scalaz.Free

object Harness {
  import HarnessOp._

  def apply[F[_], G[_]]: PartialHarness[F, G] =
    new PartialHarness[F, G]

  def pattern[F[_], G[_], A](pf: PartialFunction[F[A], G[A]]): Harness[F, G, Unit] = {
    val e = new Exception
    val trace = e.getStackTrace.toList.drop(1)

    Free.liftF(Pattern(PartialNT.broaden(pf), trace))
  }

  final class PartialHarness[F[_], G[_]] private[Harness] () {

    // copy/pasted to allow for proper stack computation
    def pattern[A](pf: PartialFunction[F[A], G[A]]): Harness[F, G, Unit] = {
      val e = new Exception
      val trace = e.getStackTrace.toList.drop(1)

      Free.liftF(Pattern(PartialNT.broaden(pf), trace))
    }
  }
}
