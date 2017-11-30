package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap



trait InteractionTest extends FunSuite with Checkers {

  test("tileLocation") {
    assert(Interaction.tileLocation(Tile(0, 0, 0)) === Location(85.05112877980659, -180.0))
    assert(Interaction.tileLocation(Tile(10, 10, 10)) === Location(84.7383871209534, -176.484375))
  }

  ignore("заполнение tiles") {
    // todo: сгенерировать по 2015
    val annualData= for (year<-(1975 to 1975)) yield
      (year,
        Extraction.locationYearlyAverageRecords( Extraction.locateTemperatures(year,"\\stations.csv",s"\\$year.csv"))
      )
    Interaction.generateTiles(annualData,Interaction.imageGenerator)

  }
}
