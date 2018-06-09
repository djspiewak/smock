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

/**
 * Equivalent to F ~> λ[α => Option[G[α]]]
 */
trait PartialNT[F[_], G[_]] {
  def apply[α](fa: F[α]): Option[G[α]]
}

object PartialNT {

  def broaden[F[_], G[_], A](pf: PartialFunction[F[A], G[A]]): PartialNT[F, G] =
    new PartialNT[F, G] {

      def apply[α](fa: F[α]): Option[G[α]] = {
        // erasure + runtime checks in isDefinedAt allow this unsound expansion
        val applied = pf.asInstanceOf[PartialFunction[F[α], G[α]]]

        if (applied.isDefinedAt(fa))
          Some(applied(fa))
        else
          None
      }
    }
}
