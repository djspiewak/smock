/*
 * Copyright 2018 Daniel Spiewak
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

import scalaz.{~>, \/, Free, Functor}
import scalaz.syntax.functor._
import scalaz.syntax.std.option._

object Harness {
  import HarnessOp._

  def apply[F[_], G[_]]: PartialHarness[F, G] =
    new PartialHarness[F, G]

  def pattern[F[_], G[_]: Functor, A](pf: PartialFunction[F[A], G[A]]): Harness[F, G, Unit] = {
    val e = new Exception
    val trace = e.getStackTrace.toList.drop(1)

    val pf2 = pf.andThen(_.map(a => ((), a)))

    Free.liftF(Pattern(PartialNT.broaden[F, λ[α => G[(Unit, α)]], A](pf2), trace))
  }

  def patternWithState[F[_], G[_], A, S](pf: PartialFunction[F[A], G[(S, A)]]): Harness[F, G, S] = {
    val e = new Exception
    val trace = e.getStackTrace.toList.drop(1)

    Free.liftF(Pattern(PartialNT.broaden[F, λ[α => G[(S, α)]], A](pf), trace))
  }

  /** Consumes steps of the program while the given partial function is defined. */
  def whileDefined[F[_], G[_], A](pf: PartialFunction[F[A], G[A]]): Harness[F, G, Unit] = {
    val e = new Exception
    val trace = e.getStackTrace.toList.drop(1)

    mkWhileDefined(pf, trace)
  }

  final class PartialHarness[F[_], G[_]] private[Harness] () {

    // copy/pasted to allow for proper stack computation
    def pattern[A](pf: PartialFunction[F[A], G[A]])(implicit G: Functor[G]): Harness[F, G, Unit] = {
      val e = new Exception
      val trace = e.getStackTrace.toList.drop(1)

      val pf2 = pf.andThen(_.map(a => ((), a)))

      Free.liftF(Pattern(PartialNT.broaden[F, λ[α => G[(Unit, α)]], A](pf2), trace))
    }

    def patternWithState[A, S](pf: PartialFunction[F[A], G[(S, A)]]): Harness[F, G, S] = {
      val e = new Exception
      val trace = e.getStackTrace.toList.drop(1)

      Free.liftF(Pattern(PartialNT.broaden[F, λ[α => G[(S, α)]], A](pf), trace))
    }

    /** Consumes steps of the program while the given partial function is defined. */
    def whileDefined[A](pf: PartialFunction[F[A], G[A]]): Harness[F, G, Unit] = {
      val e = new Exception
      val trace = e.getStackTrace.toList.drop(1)

      mkWhileDefined(pf, trace)
    }
  }

  private def mkWhileDefined[F[_], G[_], A](pf: PartialFunction[F[A], G[A]], trace: StackTrace): Harness[F, G, Unit] = {
    val f: F ~> λ[α => Unit \/ G[α]] =
      new (F ~> λ[α => Unit \/ G[α]]) {
        def apply[α](fa: F[α]): Unit \/ G[α] =
          pf.asInstanceOf[PartialFunction[F[α], G[α]]]
            .lift(fa)
            .toRightDisjunction(())
      }

    Free.liftF(WhileDefined(f, trace))
  }
}
