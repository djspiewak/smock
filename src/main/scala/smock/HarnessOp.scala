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

sealed trait HarnessOp[F[_], G[_], A] extends Product with Serializable {

  // we need to go through this somewhat sideways encoding
  // mostly because scalac can't handle gadt type unification
  def fold[B, R](k: A => B)(pattern: (Unit => B, PartialNT[F, G], StackTrace) => R): R
}

object HarnessOp {

  final case class Pattern[F[_], G[_]](pf: PartialNT[F, G], trace: StackTrace) extends HarnessOp[F, G, Unit] {
    def fold[B, R](k: Unit => B)(pattern: (Unit => B, PartialNT[F, G], StackTrace) => R): R =
      pattern(k, pf, trace)
  }
}
