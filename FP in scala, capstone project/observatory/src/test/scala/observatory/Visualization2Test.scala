package observatory

import com.sksamuel.scrimage.RGBColor
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import scala.math.pow

class Visualization2TestRunner extends Visualization2Test
trait Visualization2Test extends FunSuite with Checkers {

  def fixedTemperaturesEarth(temperature: Temperature):List[(Location,Temperature)]={
    (
    for (lat<-(90 to -90 by -1);long<-(-180 to 180))
      yield (Location(lat,long),temperature)
      ).toList
  }

  test("bilinear interpolation. point within") {
    val (d00,d01,d10,d11)=(0,0,1,1)
    val cellPoint=CellPoint(0.5,0.5)
    assert(Visualization2.bilinearInterpolation(cellPoint,d00,d01,d10,d11)==0.5)
  }

  test("bilinear interpolation. point on top margin") {
    val (d00,d01,d10,d11)=(0,0,1,1)
    val cellPoint=CellPoint(0,0.5)
    assert(Visualization2.bilinearInterpolation(cellPoint,d00,d01,d10,d11)==0)
  }

  test("visualize grid. Fixed zero-deviation Earth") {
    val temperatureOfEarth=0
    val temperatures=fixedTemperaturesEarth(temperatureOfEarth)
    val grid=Manipulation.makeGrid(temperatures)
    val tile=Tile(0,0,0)
    val image=Visualization2.visualizeGrid(grid,ColorDeviations,tile)
    assert(image.color(0,0)==RGBColor(255,255,255,Alpha),
      "image bounds color for zero temperature deviation should be Color(255,255,255)")
    assert(image.color(image.dimensions._1-1,image.dimensions._2-1)==RGBColor(255,255,255,Alpha),
      "image bounds color for zero temperature deviation should be Color(255,255,255)")

    assert(image.color(image.dimensions._1/2,image.dimensions._2/2)==RGBColor(255,255,255,Alpha),
      "image center color for zero temperature deviation should be Color(255,255,255)")
  }

  test("visualize grid. All fixed-deviation Earth alternatives") {
    for( (temperatureOfEarth,colorOfEarth) <-ColorDeviations) {
      val temperatures = fixedTemperaturesEarth(temperatureOfEarth)
      val grid = Manipulation.makeGrid(temperatures)
      val tile = Tile(0, 0, 0)
      val image = Visualization2.visualizeGrid(grid, ColorDeviations, tile)

      assert(image.color(0, 0) == (colorOfEarth match {case Color(r,g,b)=>RGBColor(r,g,b,Alpha)}),
        s"image bounds color for zero temperature deviation should be $colorOfEarth")
      assert(image.color(image.dimensions._1 -1, image.dimensions._2 -1) ==  (colorOfEarth match {case Color(r,g,b)=>RGBColor(r,g,b,Alpha)}),
        s"image bounds color for zero temperature deviation should be $colorOfEarth")
      assert(image.color(image.center._1, image.center._2) ==  (colorOfEarth match {case Color(r,g,b)=>RGBColor(r,g,b,Alpha)}),
        s"image center color for zero temperature deviation should be $colorOfEarth")
    }
  }

  test("generate deviations images") {

    Visualization2.deviationTilesGenerator(2010,2015)

  }
}
