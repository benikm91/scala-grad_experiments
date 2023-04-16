package scalagrad.showcase.deeplearning

object Util:

    def time[R](block: => R): R = {
        val t0 = System.nanoTime()
        val result = block    // call-by-name
        val t1 = System.nanoTime()
        val ds = (t1 - t0) / 1000000
        println("Elapsed time: " + (ds) + "s")
        result
    }
