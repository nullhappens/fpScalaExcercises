object MyModule {
	def abs(n:Int): Int =
		if (n < 0) -n
		else n

	def factorial (n:Int): Int = {
		@annotation.tailrec
		def go(n:Int, acc:Int): Int =
			if (n <= 0) acc
			else go(n-1, n*acc)
		go(n,1)
	}
	//non tail recursive fibonacci
	def fib (n:Int): Int = {
		if (n <= 1) n
		else fib(n-1) + fib(n-2)
	}
	//tail recursive fibonacci
	def fib2 (n:Int): Int = {
		@annotation.tailrec
		def go(i: Int, acc: Int, x: Int): Int =
			if (i <= 0) acc
			else go(i - 1, x, acc + x)
		go(n, 0, 1)
	}
	private def formatAbs(x:Int): String = {
		val msg = "The absolute value of %d is %d"
		msg.format(x, abs(x))
	}

	def main (args: Array[String]): Unit =
		println(fib(2))
		println(fib(3))
		println(fib2(2))
		println(fib2(3))
	}