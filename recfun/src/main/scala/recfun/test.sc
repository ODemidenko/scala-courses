
def calc(f:(Int,Int)=>Int,t:Int)(a:Int,b:Int):Int=
  if (a>b) t
  else f(a,calc(f,t)(a+1,b))

calc((x,y)=>x*y,1)(1,5)


def mapReduce(f:(Double)=>Double,r:(Double,Double)=>Double,termVal:Double, n:Double):Double=
{
  def loop(a:Double):Double=
    if (a>n) termVal
    else r(f(a),loop(a+1))
  loop(1.0)
}


mapReduce((x)=>x*x,(x,y)=>x+y,0,4)

