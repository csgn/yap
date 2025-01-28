package yap
package benchmarks

import scala.util.Random

import org.scalameter._

object StackBenchmark extends Bench.LocalTime {
  val sizes: Gen[Int] = Gen.range("size")(1000, 10000, 1000)

  val stacks: Gen[Stack[Int]] = for {
    size <- sizes
  } yield Stack((1 to size): _*)

  // Benchmark for push operation
  performance.of("Stack") in {
    measure.method("push") in {
      using(stacks) in { stack =>
        stack.push(Random.nextInt())
      }
    }
  }

  // Benchmark for pop operation
  performance.of("Stack") in {
    measure.method("pop") in {
      using(stacks) in { stack =>
        stack.pop
      }
    }
  }

  // Benchmark for peek operation
  performance.of("Stack") in {
    measure.method("peek") in {
      using(stacks) in { stack =>
        stack.peek
      }
    }
  }

  // Benchmark for isEmpty operation
  performance.of("Stack") in {
    measure.method("isEmpty") in {
      using(stacks) in { stack =>
        stack.isEmpty
      }
    }
  }
}
