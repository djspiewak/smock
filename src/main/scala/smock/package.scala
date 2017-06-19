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

import scalaz.{~>, -\/, \/-, Catchable, Coyoneda, Forall, Free, Monad}
import scalaz.syntax.monad._

import org.specs2.execute._

package object smock {
  type Harness[F[_], G[_], A] = Free[HarnessOp[F, G, ?], A]

  final implicit class HarnessSyntax[F[_], G[_], A](val self: Harness[F, G, A]) extends AnyVal {
    import StandardResults._

    // an asserted foldMap
    def interpret[B](target: Free[F, B])(implicit GM: Monad[G], GC: Catchable[G]): G[B] = {
      def inner(self: Free[Coyoneda[HarnessOp[F, G, ?], ?], A], target: Free[Coyoneda[F, ?], B]): G[B] = {
        (self.resume, target.resume) match {
          case (-\/(h), -\/(s)) =>
            val (gs, self2) = h.fi.fold(h.k)(
              pattern = { (k, pf) =>
                val gs = pf(s.fi) match {
                  case Some(gi) =>
                    gi.map(s.k)

                  case None =>
                    // TODO line numbers
                    GC.fail(FailureException(failure(s"unexpected suspension: ${s.fi}")))
                }

                (gs, k(()))
              })

            gs.flatMap(fc => inner(self2, fc))

          case (-\/(_), \/-(_)) =>
            GC.fail(FailureException(failure("early program termination")))   // TODO line number of the harness suspension where we failed

          case (\/-(_), -\/(s)) =>
            GC.fail(FailureException(failure(s"unexpected trailing suspension: ${s.fi}")))

          case (\/-(_), \/-(b)) =>
            Monad[G].point(b)
        }
      }

      val selfLifted =
        self.mapSuspension(λ[HarnessOp[F, G, ?] ~> Coyoneda[HarnessOp[F, G, ?], ?]](Coyoneda.lift(_)))

      val targetLifted =
        target.mapSuspension(λ[F ~> Coyoneda[F, ?]](Coyoneda.lift(_)))

      inner(selfLifted, targetLifted)
    }
  }

  private[smock] type ∀[F[_]] = Forall[F]
}
