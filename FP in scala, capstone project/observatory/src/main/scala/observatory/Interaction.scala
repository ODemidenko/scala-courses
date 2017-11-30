package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    tile.toLatLon
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val tilePixelScale = tile.zoom+1
    val latPixelScale=(tile.zoom*2)/180/256
    val lonPixelScale=(tile.zoom*2)/360/256
    //// Так можно делать присваивание ведь?
    val Location(latB,lonB)=tileLocation(tile)
    val tileContent=for (pHeight<-(0 until 256) ; pWidth<-(0 until 256))
    yield {
      /*each tile consists of pixels (256*256), where puxel coordinates may be considered being 8-times smaller tiles
        as 256=2**8
      */
      val location=Tile(256*tile.x+pWidth,256*tile.y+pHeight,tile.zoom*8).toLatLon
      Visualization.interpolateColor(colors,
        Visualization.predictTemperature(temperatures,
          location))
      match {
        case Color(r, g, b) => Pixel(r, g, b, 100)
      }
    }
      Image (256, 256, tileContent.toArray: Array[Pixel] )
  }

  /**
  * function of generateImage signature, which I can use localy to generate Tile images
   */
  def imageGenerator(year:Year, drawingTile:Tile, yearData:Iterable[(Location, Temperature)]):Unit={

    val outputFile = new java.io.File(s"target//temperatures//$year//${drawingTile.zoom}//${drawingTile.x}-${drawingTile.y}.png")
    outputFile.getParentFile.mkdirs()

    val image:Image=tile(yearData,ColorScale,drawingTile)
    image.output(outputFile)
  }

  //todo: somewhere must exist a function, calling generation of yearly data
  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for ((year,data)<-yearlyData.par) {
      for (zoom<-(0 to 3); x<-(0 until pow(zoom.toDouble,2d).toInt); y<-(0 until pow(zoom.toDouble,2d).toInt) ) {
        generateImage(year,Tile(x,y,zoom),data)
      }
    }
  }

}
