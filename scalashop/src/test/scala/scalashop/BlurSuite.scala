package scalashop

import java.util.concurrent.*
import scala.collection.*

class BlurSuite extends munit.FunSuite:
    // Put tests here
    test("the dollar") {
        val radius = 3
        val width = 1920
        val height = 1080
        val src = Img(width, height)
        val dst = Img(width, height)
        VerticalBoxBlur.parBlur(src, dst, 4, radius)

//        val numTasks = 32
//        val partime = standardConfig measure {
//            VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
//        }
//        println(s"fork/join blur time: $partime")
//        println(s"speedup: ${seqtime.value / partime.value}")
    }
