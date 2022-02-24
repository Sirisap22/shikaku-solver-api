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
      sum += runtime
    }
    val averageRuntime = sum / n
    averageRuntime
  } 

  def averageRuntimeInMilliSec[R](n: Int)(block: => R): Double = {
    averageRuntimeInNanoSec(n)(block)/1000000
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

  def averageUsedMemory[R](time: Int, divider: Int)(block: => R): (R, Double) = {
    val runtime = Runtime.getRuntime
    val m1 = runtime.totalMemory() - runtime.freeMemory()
    val result = block
    val m2 = runtime.totalMemory() - runtime.freeMemory()

    return (result, (m2 - m1).toDouble/(divider*time))
  }
}
