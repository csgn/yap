package yap
package benchmarks

import scala.util.Random
import org.scalameter._

object LinkBenchmark extends Bench.LocalTime {
  val sizes: Gen[Int] = Gen.range("size")(1000, 10000, 1000)

  val links: Gen[(Int, Link[Int])] = for {
    size <- sizes
  } yield (size, Link((1 to size): _*))

  // Benchmark for map operation
  performance.of("Link") in {
    measure.method("map") in {
      using(links) in { case (_, link) =>
        link.map(_ * 2)
      }
    }
  }

  // Benchmark for flatMap operation
  performance.of("Link") in {
    measure.method("flatMap") in {
      using(links) in { case (_, link) =>
        link.flatMap(el => Link(el, el + 1))
      }
    }
  }

  // Benchmark for prepend operation
  performance.of("Link") in {
    measure.method("prepend") in {
      using(links) in { case (_, link) =>
        link.prepend(Random.nextInt())
      }
    }
  }

  // Benchmark for append operation
  performance.of("Link") in {
    measure.method("append") in {
      using(links) in { case (_, link) =>
        link.append(Random.nextInt())
      }
    }
  }

  // Benchmark for search operation
  performance.of("Link") in {
    measure.method("search") in {
      using(links) in { case (size, link) =>
        link.search(size / 2)
      }
    }
  }

  // Benchmark for foldLeft operation
  performance.of("Link") in {
    measure.method("foldLeft") in {
      using(links) in { case (_, link) =>
        link.foldLeft(0)(_ + _)
      }
    }
  }

  // Benchmark for foldRight operation
  performance.of("Link") in {
    measure.method("foldRight") in {
      using(links) in { case (_, link) =>
        link.foldRight(0)(_ + _)
      }
    }
  }

  // Benchmark for reverse operation
  performance.of("Link") in {
    measure.method("reverse") in {
      using(links) in { case (_, link) =>
        link.reverse
      }
    }
  }

  // Benchmark for remove operation
  performance.of("Link") in {
    measure.method("remove") in {
      using(links) in { case (size, link) =>
        link.remove(size / 2)
      }
    }
  }

  // Benchmark for removeAt operation
  performance.of("Link") in {
    measure.method("removeAt") in {
      using(links) in { case (size, link) =>
        link.removeAt(size / 2)
      }
    }
  }
}
