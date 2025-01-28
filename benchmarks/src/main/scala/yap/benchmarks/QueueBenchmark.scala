package yap
package benchmarks

import scala.util.Random

import org.scalameter._

object QueueBenchmark extends Bench.LocalTime {
  val sizes: Gen[Int] = Gen.range("size")(1000, 10000, 1000)

  val queues: Gen[Queue[Int]] = for {
    size <- sizes
  } yield Queue((1 to size): _*)

  performance.of("Queue") in {
    measure.method("enqueue") in {
      using(queues) in { queue =>
        queue.enqueue(Random.nextInt())
      }
    }
  }

  // Benchmark for dequeue operation
  performance.of("Queue") in {
    measure.method("dequeue") in {
      using(queues) in { queue =>
        queue.dequeue
      }
    }
  }

  // Benchmark for isEmpty operation
  performance.of("Queue") in {
    measure.method("isEmpty") in {
      using(queues) in { queue =>
        queue.isEmpty
      }
    }
  }
}
