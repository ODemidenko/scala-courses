package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap


class InterationTestRunner extends InteractionTest
trait InteractionTest extends FunSuite with Checkers {

  test("tileLocation") {
    assert(Interaction.tileLocation(Tile(0, 0, 0)) === Location(85.05112877980659, -180.0))
    assert(Interaction.tileLocation(Tile(10, 10, 10)) === Location(84.7383871209534, -176.484375))
  }

  test("tile coordinates,zooming is correct") {
    assert(Tile(8,0,3).location.lon==180)
    assert(Tile(8,8,3).location.lat < -80)
  }


  test("заполнение tiles") {
    // todo: сгенерировать c 1975 по 2015
    val annualData= for (year<-(2010 to 2015)) yield
      (year,
        Extraction.locationYearlyAverageRecords( Extraction.locateTemperatures(year,"\\stations.csv",s"\\$year.csv"))
      )
    Interaction.generateTiles(annualData,Interaction.imageGenerator)
  }
}
