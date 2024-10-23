package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

	@volatile var seqResult = false

	@volatile var parResult = false

	val standardConfig = config(
		Key.exec.minWarmupRuns := 40,
		Key.exec.maxWarmupRuns := 80,
		Key.exec.benchRuns := 120,
		Key.verbose := false
	) withWarmer (Warmer.Default())

	def main(args: Array[String]): Unit =
		val length = 100000000
		val chars = new Array[Char](length)
		val threshold = 10000
		val seqtime = standardConfig measure {
			seqResult = ParallelParenthesesBalancing.balance(chars)
		}
		println(s"sequential result = $seqResult")
		println(s"sequential balancing time: $seqtime")

		val fjtime = standardConfig measure {
			parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
		}
		println(s"parallel result = $parResult")
		println(s"parallel balancing time: $fjtime")
		println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:

	/** Returns `true` iff the parentheses in the input `chars` are balanced.
	 */
	def balance(chars: Array[Char]): Boolean = {
		val v: Int = chars.foldLeft(0)((a: Int, b: Char) => {
			if (b == '(')
				a + 1
			else if (b == ')')
				if (a == 0)
					(-chars.length) - 1 // prevents from ever going positive, but can overflow
				else
					a - 1
			else
				a
		})
		(v == 0)
	}

	/** Returns `true` iff the parentheses in the input `chars` are balanced.
	 */
	def parBalance(chars: Array[Char], threshold: Int): Boolean =
		println(chars.mkString("Array(", ", ", ")"))
		def traverse(idx: Int, until: Int): Int = {
			if(idx < chars.length)
				if(until - idx > 0)
					val cs: Array[Char] = chars.slice(idx, until)
					cs.foldLeft(0)((n: Int, c: Char) => {
						if (c == '(')
							n + 1
						else if (c == ')')
							if (n < 0)
								-chars.length
							else
								n - 1
						else
							n
					})
				else
					if(chars(idx) == '(')
						1
					else if(chars(idx) == ')')
						-1
					else
						0
			else
				0
		}

//		print(chars.mkString("Array(", ", ", ")"))
		def reduce(from: Int, until: Int): Int = {
			val size: Int = until - from
			if(size < threshold)
				traverse(from, until)
			else
				val mid: Int = from + ((until - from) / 2)
				val i: (Int, Int) = parallel( reduce(from, mid), reduce(mid + 1, until) )
				val r: Int = i._1 + i._2
				if(i._1 < 0 || r < 0)
					-chars.length
				else
					r
		}

		val r: Int = reduce(0, chars.length)
		r == 0

