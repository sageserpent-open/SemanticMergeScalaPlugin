import scala.annotation.tailrec
val x = 2

println("Testing")

1 to 10 sum

@tailrec
def aFunction(x: String): String = if (x.size < 100) aFunction(s"($x-$x)") else x

aFunction("")