package observatory


import com.sksamuel.scrimage.Image
import observatory.Interaction.tile
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

class VisualizationTestRunner extends VisualizationTest
trait VisualizationTest extends FunSuite with Checkers {

  test("predict temperature, with exact location match") {
    val doubleTemp=List(
      (Location(0,0),0.0),
      (Location(0,20),10.0)
    )
    assert(Visualization.predictTemperature(doubleTemp,Location(0,0))==0.0, "temperature localization by exact match failed")
  }

  test("predict temperature at 0 longitude, with west and east latitude points") {
    val doubleTemp = List(
      (Location(0, -10), 0.0),
      (Location(0, 10), 10.0)
    )
    assert(Visualization.predictTemperature(doubleTemp, Location(0, 0)).round == 5.0, "temperature localization by 2 points failed")
  }
  test("predict temperature at -180 longitude, with west and east latitude points") {
    val doubleTemp = List(
      (Location(0, -179), 0.0),
      (Location(0, 179), 10.0)
    )
    assert(Visualization.predictTemperature(doubleTemp, Location(0, 180)).round == 5.0, "temperature localization by 2 points failed")
  }
  test("predict temperature, around the pole") {
    val doubleTemp = List(
      (Location(80, 180), 0.0),
      (Location(80, 0), 10.0)
    )
    assert(Visualization.predictTemperature(doubleTemp, Location(90, 0)).round == 5.0, "temperature localization by 2 points failed")
  }

 test("predict temperature, with even distances to all data points") {
   val doubleTemp=List(
     (Location(0,0),0.0),
     (Location(0,20),10.0)
   )
   assert(Visualization.predictTemperature(doubleTemp,Location(0,10)).round==5.0, "temperature localization by 2 points failed")

   val fourTemp=List(
     (Location(0,0),10.0),
     (Location(0,20),20.0),
     (Location(10,0),10.0),
     (Location(10,20),20.0)
   )
   assert(Visualization.predictTemperature(fourTemp,Location(5,10))==15.0, "temperature localization by 4 points failed")
//   println(Visualization.predictTemperature(fourTemp,Location(10,19)))
 }

  test("predict temperature with uneven distances between data points") {
    val doubleTemp=List(
      (Location(0,0),0.0),
      (Location(0,20),10.0)
    )
    assert(Visualization.predictTemperature(doubleTemp, Location(0, 18)).floor == 9.0, "temperature localization by 2 points failed")
  }

  test("interpolate colors") {
    val predictedColor=Visualization.interpolateColor(ColorScale,22.0)
    val expectedColor=Color(255,128,0)
    assert(predictedColor==expectedColor,s"temperature color failed. expected: $expectedColor, received: $predictedColor")
  }
  test("interpolate colors with above expectation temperatures") {
    val predictedColor=Visualization.interpolateColor(ColorScale,100.0)
    val expectedColor=Color(255,255,255)
    assert(predictedColor==expectedColor,"exceeding the greatest value of a color scale should return the color associated with the greatest value")
  }
  test("interpolate colors in between") {
    val predictedColor=Visualization.interpolateColor(ColorScale,7)
    assert(predictedColor.red>predictedColor.blue,s"got: $predictedColor Expected to be closer to Color(255,255,0) than Color(0,255,255)")
  }

  test("visualize") {
    TileScale=128
    val year=2015
    val image=Visualization.visualize(
      Extraction.locationYearlyAverageRecords( Extraction.locateTemperatures(year,"\\stations.csv",s"\\$year.csv")),
      ColorScale
    )
    val outputFile = new java.io.File(s"target//image-test.png")
    image.output(outputFile)
  }

}
