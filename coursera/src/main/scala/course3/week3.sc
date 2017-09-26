def initializeArray(xs: Array[Int])(v: Int): Unit = {
  for (i <- (0 until xs.length).par) {
    xs(i) = v
  }
}


val xs = new Array[Int](100000001)
(1 to 100000).foreach(i => xs(i) = i)
val start = System.currentTimeMillis()
initializeArray(xs)(25)
val end = System.currentTimeMillis()
val dur = end - start


xs

/**
  * Mandelbrot set
 */

/**
private def computePixel(xc: Double, yc: Double, maxIterations: Int): Int = {
  var i = 0
  var x, y = 0.0
  while (x * x + y * y < 4 && i < maxIterations) {
    val xt = x * x - y * y + xc
    val yt = 2 * x * y + yc
    x = xt; y = yt
    i += 1
  }
  color(i)
}

def parRender(): Unit = {
  for (idx <- (0 until image.length).par) {
    val (xc, yc) = coordinatesFor(idx)
    image(idx) = computePixel(xc, yc, maxIterations)
  }
}
*/

