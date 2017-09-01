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

import cats.effect.IO
import cats.free.Free
import cats.implicits._
import org.specs2.execute.FailureException
import org.specs2.mutable._

object HarnessSpec extends Specification {
  type FileSystem[A] = Free[FileSystemOp, A]

  "free test harness" should {
    import FileSystemOp._

    "check a single-suspension program" in {
      val H = Harness[FileSystemOp, IO]

      val harness = for {
        _ <- H.pattern[Array[Byte]] {
          case Read(name) =>
            IO {
              name mustEqual "foo.txt"

              Array(1, 2, 3, 4, 5)
            }
        }
      } yield ()

      harness(read("foo.txt")).unsafeRunSync() mustEqual Array(1, 2, 3, 4, 5)
    }

    "check a multi-suspension program" in {
      val H = Harness[FileSystemOp, IO]

      val harness = for {
        _ <- H.pattern[Array[Byte]] {
          case Read(name) =>
            IO {
              name mustEqual "foo.txt"

              Array(1, 2, 3, 4, 5)
            }
        }

        _ <- H.pattern[Unit] {
          case Write(name, contents) =>
            IO {
              name mustEqual "bar.txt"
              contents mustEqual Array(3, 2, 1)
            }
        }
      } yield ()

      harness(read("foo.txt") >> write("bar.txt", Array(3, 2, 1))).unsafeRunSync()

      ok
    }

    "fail on an early-terminating program" in {
      val H = Harness[FileSystemOp, IO]

      val harness = for {
        _ <- H.pattern[Array[Byte]] {
          case Read(name) =>
            IO {
              name mustEqual "foo.txt"

              Array(1, 2, 3, 4, 5)
            }
        }

        _ <- H.pattern[Unit] {
          case Write(_, _) => IO(())
        }
      } yield ()

      {
        harness(read("foo.txt")).unsafeRunSync() mustEqual Array(1, 2, 3, 4, 5)
      } must throwA[FailureException]
    }

    "fail when receiving the wrong suspension" in {
      val H = Harness[FileSystemOp, IO]

      val harness = for {
        _ <- H.pattern[Unit] {
          case Write(_, _) => IO(())
        }
      } yield ()

      {
        harness(read("foo.txt")).unsafeRunSync() mustEqual Array(1, 2, 3, 4, 5)
      } must throwA[FailureException]
    }
  }

  sealed trait FileSystemOp[A] extends Product with Serializable

  object FileSystemOp {
    final case class Read(name: String) extends FileSystemOp[Array[Byte]]

    def read(name: String): FileSystem[Array[Byte]] =
      Free.liftF(Read(name))

    final case class Write(name: String, contents: Array[Byte]) extends FileSystemOp[Unit]

    def write(name: String, contents: Array[Byte]): FileSystem[Unit] =
      Free.liftF(Write(name, contents))
  }
}
