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

  def trace: StackTrace

  // we need to go through this somewhat sideways encoding
  // mostly because scalac can't handle gadt type unification
  def fold[B, R](
      k: A => B)(
      pattern: ∀[λ[β => (β => B, PartialNT[F, λ[α => G[(β, α)]]]) => R]]): R
}

object HarnessOp {

  final case class Pattern[F[_], G[_], S](
      pf: PartialNT[F, λ[α => G[(S, α)]]],
      trace: StackTrace) extends HarnessOp[F, G, S] {

    def fold[B, R](
        k: S => B)(
        pattern: ∀[λ[β => (β => B, PartialNT[F, λ[α => G[(β, α)]]]) => R]]): R =
      pattern[S](k, pf)
  }
}
