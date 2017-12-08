package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._
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

      (
        if (longDelta==0 && (loc1.lat==loc2.lat)) 0
        else if (longDelta==180 && loc1.lat== -loc2.lat) math.Pi
        else
         acos(
            sin(loc1.lat.toRadians)*sin(loc2.lat.toRadians)+
            cos(loc1.lat.toRadians)*cos(loc2.lat.toRadians)*cos(longDelta.toRadians)
          )
        )*6373
    }

    val threshold=1 //for points closer than threshold, given in km, their value will be used
    val p=2.5 //fudge factor for inverse distance weighting

    val distAndTemp = for((loc,temp)<-temperatures) yield (distance(location,loc),temp)
    val distanceWightedSum=distAndTemp.foldLeft(0d){case(sum,(distance,temp))=> sum+ temp/pow(distance,p)}
    val invertDistanceSum=distAndTemp.foldLeft(0d){case(sum,(distance,_))=>sum+1/pow(distance,p)}

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
//      val colorRange=greater-smaller
//      val myInterpolation=(colorRange*lowerDif/(greaterDif+lowerDif)+smaller).round.toInt

      val totalRange=lowerDif+greaterDif
      val wikiInterpolation=(smaller*(1-lowerDif/totalRange)+greater*lowerDif/totalRange).round.toInt
//      assert(myInterpolation==wikiInterpolation)
      wikiInterpolation
    }
    /*
    Divide set of points into RGB channels. find either the closest or identical value,
    with temperatures higher and lower. Take linear mean of their RGB values
         */
    val sortedSeq=points.toSeq.sortBy {case (temp,_)=>temp}
    val (lowerTemp,greaterTemp) = sortedSeq.span {case (temp,_)=>temp<value}

    if (lowerTemp.isEmpty) sortedSeq.head._2
    else if (greaterTemp.isEmpty) sortedSeq.last._2
    else if (greaterTemp.head._1==value) greaterTemp.head._2
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

//    т.  к. окружность 360 градусов, то долготу -180 (=180) нужно посчитать только один раз! Аналогично полюс (90) нужно посчитать однократно
//    val pixels=for (long<-(-179 to 180);lat<-(90 to -89 by -1) if long>0 || (lat!=90 && lat != -90))
    val pixels=for (lat<-(90 to -90 by -1);long<-(-180 to 180))
        yield interpolateColor(colors,predictTemperature(temperatures,Location(lat,long))) match
        {case Color(r,g,b)=>Pixel(r,g,b,100)}
    Image(361,181,pixels.toArray)
}
}

