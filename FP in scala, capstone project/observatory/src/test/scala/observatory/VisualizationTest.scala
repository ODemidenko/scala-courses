package observatory


import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait VisualizationTest extends FunSuite with Checkers {
 test("predict temperature") {
   val doubleTemp=List(
     (Location(0,0),0.0),
     (Location(0,20),10.0)
   )
   assert(Visualization.predictTemperature(doubleTemp,Location(0,10))==5.0, "temperature localization by 2 points failed")
   assert(Visualization.predictTemperature(doubleTemp,Location(0,0))==0.0, "temperature localization by exact match failed")

   val fourTemp=List(
     (Location(0,0),10.0),
     (Location(0,20),20.0),
     (Location(10,0),10.0),
     (Location(10,20),20.0)
   )
   assert(Visualization.predictTemperature(fourTemp,Location(5,10))==15.0, "temperature localization by 4 points failed")
 }

  test("interpolate colors") {
    val predictedColor=Visualization.interpolateColor(ColorScale,22.0)
    val expectedColor=Color(255,127,0)
    assert(predictedColor==expectedColor,s"temperature color failed. expected: $expectedColor, received: $predictedColor")
  }

}
