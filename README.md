# smock [![Build Status](https://travis-ci.org/djspiewak/smock.svg?branch=master)](https://travis-ci.org/djspiewak/smock)

A relatively-trivial library for testing `Free` programs, inspired by [purescript-mockfree](https://github.com/slamdata/purescript-mockfree) and the testing internals within [Nelson](https://github.com/Verizon/nelson/tree/master/core/src/test/scala/test).

## Usage

```sbt
libraryDependencies += "com.codecommit" %% "smock" % "0.4.1-specs2-4.2.0" % "test"
```

Depends on **scalaz 7.2.18**.  Cross-builds are available for Scala 2.12 and 2.11.  Different builds are published for various specs2 versions.  Specifically:

- Specs2 4.2.0
  + Scala 2.11
  + Scala 2.12
- Specs2 4.0.2
  + Scala 2.11
  + Scala 2.12
- Specs2 3.9.1
  + Scala 2.11
  + Scala 2.12
- Specs2 3.8.4
  + Scala 2.11

A quick example stolen from the current version of the spec:

```scala
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

      harness(read("foo.txt") >> write("bar.txt", Array(3, 2, 1))).unsafePerformIO()

      ok
    }
```

The program under test is on the penultimate line (`read >> write`).  The harness we construct in the `for`-comprehension asserts that the first suspension is a `Read`, and the second suspension is a `Write`, and there are no further suspensions.  When the assertion is passed, we have the opportunity to run further assertions (within the side-effect lifted into `IO`) as well as the ability to produce the value to which the suspension will interpret.

Further features of this framework (such as support for branching workflows, or state passing) are very possible, but out of scope for this afternoon.
We 
