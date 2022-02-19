package shikaku

object Utils {

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println(s"Elapsed time: ${t1 - t0} ns")
    result
  }

  def timer[R](block: => R): Long = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    t1 - t0
  }

  def averageRuntimeInNanoSec[R](n: Int)(block: => R): Double = {
    var sum = 0D
    for (i <- 0 until n) {
      val runtime = timer(block)
      // println(s"Round $i: $runtime ns")
      sum += runtime
    }
    val averageRuntime = sum / n
    // println(s"Average Runtime: $averageRuntime ns")
    averageRuntime
  } 

  def memory(): Unit = {
        val mb = 1024*1024
        val runtime = Runtime.getRuntime
        println("\nMemory in MB")
        println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory)/mb)
        println("** Free Memory:  " + runtime.freeMemory/mb)
        println("** Total Memory: " + runtime.totalMemory/mb)
        println("** Max Memory:   " + runtime.maxMemory/mb)
    }
}
