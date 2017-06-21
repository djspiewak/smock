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

    private type FreeC[H[_], B] = Free[Coyoneda[H, ?], B]

    // an asserted foldMap
    def apply[B](target: Free[F, B])(implicit GM: Monad[G], GC: Catchable[G]): G[B] = {
      type Self = FreeC[HarnessOp[F, G, ?], A]
      type Target = FreeC[F, B]

      def inner(self: Self, target: Target): G[B] = {
        (self.resume, target.resume) match {
          case (-\/(h), -\/(s)) =>
            /*
             * Ok, let's unpack the following…
             *
             * - The intermediate type (the first parameter of the fold) is the
             *   coyoneda transformation.  Which is to say, given the existential
             *   produced value, wrap it up and get back into Free for the next
             *   iteration.
             * - The return type (the second parameter of the fold) is the
             *   pair of "next" `self` and `target` wrapped within G.
             */
            val stuffG = h.fi.fold(h.k)(
              pattern = new ∀[λ[β => (β => Self, PartialNT[F, λ[α => G[(β, α)]]]) => G[(Self, Target)]]] {
                def apply[β] = { (k, pf) =>
                  // k: β => Self
                  // pf: PartialNT[F, λ[α => G[(β, α)]]]

                  pf(s.fi) match {
                    case Some(gsi) => // gsi: G[(β, s.I)]
                      gsi map {
                        case (state, result) => (k(state), s.k(result))
                      }

                    case None =>
                      val f = Failure(s"unexpected suspension: ${s.fi}", stackTrace = h.fi.trace)
                      GC.fail(FailureException(f))
                  }
                }
              })

            stuffG flatMap {
              case (self2, fc) => inner(self2, fc)
            }

          case (-\/(h), \/-(_)) =>
            val f = Failure("unexpected program termination", stackTrace = h.fi.trace)
            GC.fail(FailureException(f))

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
  private[smock] type StackTrace = List[StackTraceElement]
}
