import scalashop._

val src = new Img(5, 5)

for (x <- 0 until 5; y <- 0 until 5)
  src(x, y) = rgba(x, y, x + y, math.abs(x - y))

red(src(1,1))
green(src(1,1))
blue(src(1,1))
alpha(src(1,1))

src(4,4)=2100483000
red(src(4,4))
green(src(4,4))
blue(src(4,4))
alpha(src(4,4))

