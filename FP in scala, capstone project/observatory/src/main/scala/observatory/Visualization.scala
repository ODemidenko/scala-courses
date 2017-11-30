package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math
/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */

  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    def distance(loc1:Location,loc2:Location):Double ={
      val longDeltaAbs=math.abs(loc1.lon - loc2.lon)
      val longDelta= if (longDeltaAbs>180) 360-longDeltaAbs else longDeltaAbs
      if (longDelta==0 && (loc1.lat==loc2.lat)) 0
      else if (longDelta==180 && loc1.lat== -loc2.lat) math.Pi
      else
        math.pow(
          math.cos(math.sin(loc1.lat)*math.sin(loc2.lat)+
            math.cos(loc1.lat)*math.cos(loc2.lat)),
          -1)
    }

    val threshold=1 //check p-value meaning

    val distAndTemp = for((loc,temp)<-temperatures) yield (distance(location,loc),temp)
    val distanceWightedSum=distAndTemp.foldLeft(0d){case(sum,(distance,temp))=> sum+ 1/distance*temp}
    val invertDistanceSum=distAndTemp.foldLeft(0d){case(sum,(distance,_))=>sum+1/distance}

    val closePoint=distAndTemp.find{case(dist,_)=> dist<threshold}
    if (closePoint.isDefined) closePoint.get match {case(dist,temp)=>temp}
    else distanceWightedSum/invertDistanceSum
  }


  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    def interpolateColorChannel(smaller:Int,greater:Int,lowerDif:Double,greaterDif:Double): Int = {
      val colorRange=greater-smaller
      (colorRange*lowerDif/(greaterDif+lowerDif)+smaller).toInt
    }
    /*
    Divide set of points into RGB channels. find either the closest or identical value,
    with temperatures higher and lower. Take linear mean of their RGB values
         */
    val sortedSeq=points.toSeq.sortBy {case (temp,_)=>temp}
    val (lowerTemp,greaterTemp) = sortedSeq.span {case (temp,_)=>temp<value}

    if (greaterTemp.head._1==value) greaterTemp.head._2
    else {
      val lowerDif = value - lowerTemp.last._1
      val greaterDif = greaterTemp.head._1 - value
      (lowerTemp.last._2,greaterTemp.head._2) match {
        case (Color(sRed,sGreen,sBlue),Color(gRed,gGreen,gBlue))=>
          Color(
            interpolateColorChannel(sRed,gRed,lowerDif,greaterDif),
            interpolateColorChannel(sGreen,gGreen,lowerDif,greaterDif),
            interpolateColorChannel(sBlue,gBlue,lowerDif,greaterDif)
          )
      }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {

    val pixels=for (lat<-(-90 to 90); long<-(-180 to 180))
        yield interpolateColor(colors,predictTemperature(temperatures,Location(lat,long))) match
        {case Color(r,g,b)=>Pixel(r,g,b,100)}
    Image(360,180,pixels.toArray:Array[Pixel])
  }

}

