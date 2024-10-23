package kmeans

import java.util.concurrent.*
import scala.collection.{mutable, Map, Seq}
import scala.collection.parallel.{ParMap, ParSeq}
import scala.collection.parallel.CollectionConverters.*
import scala.math.*

class KMeansSuite extends munit.FunSuite:
	object KM extends KMeans

	import KM.*

	def checkParClassify(points: ParSeq[Point], means: ParSeq[Point], expected: ParMap[Point, ParSeq[Point]]): Unit =
		assertEquals(classify(points, means), expected, s"classify($points, $means) should equal to $expected")

	test("'classify' should work for empty 'points' and empty 'means'") {
		val points: ParSeq[Point] = IndexedSeq().par
		val means: ParSeq[Point] = IndexedSeq().par
		val expected = ParMap[Point, ParSeq[Point]]()
		checkParClassify(points, means, expected)
	}

	test("The dollar") {
		val ps: ParSeq[Point] = ParSeq(Point(1, 1, 0), Point(1, -1, 0), Point(-1, 1, 0), Point(-1, -1, 0))
		val ms: ParSeq[Point] = ParSeq(Point(0, 0, 0))
		val r: ParMap[Point, ParSeq[Point]] = classify(ps, ms)
		val expected: ParMap[Point, ParSeq[Point]] = ParMap(
			Point(0.0, 0.0, 0.0) -> ParSeq(Point(-1.0, 0.0, 0.0), Point(-1.0, 0.0, 0.0)),
			Point(1.0, 0.0, 0.0) -> ParSeq(Point(1.0, 0.0, 0.0), Point(1.0, 0.0, 0.0))
		)
		assert(r == expected, s"\nWANT: ${expected.toString}\nGOT: ${r.toString}")
	}

	import scala.concurrent.duration.*

	override val munitTimeout = 10.seconds


