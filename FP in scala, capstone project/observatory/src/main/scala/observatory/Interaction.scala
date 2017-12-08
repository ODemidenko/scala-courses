package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.collection.mutable.ArrayBuffer
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
    tile.location
  }

  def tileCoordinatesArray(tile:Tile): Array[Location] ={
    val tilesLocsArray=Array.ofDim[Location](TileScale*TileScale)

    var pHeight=0
    while (pHeight<TileScale) {
      var pWidth = 0
      while (pWidth < TileScale) {
        tilesLocsArray(pHeight*TileScale+pWidth)=
          Tile.locationByTileParams(TileScale* tile.x + pWidth,
            TileScale * tile.y + pHeight,
          tile.zoom
            + (log(TileScale.toDouble) / log(2)
            ).toInt
        )
        pWidth+=1
      }
      pHeight+=1
    }
    tilesLocsArray
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile
          ): Image = {
    val tileContent: Array[Color] =tileCoordinatesArray(tile).map(loc=>
      Visualization.interpolateColor(colors,
        Visualization.predictTemperature(temperatures,
          loc)))
    val tileArray=tileContent.map {
        case Color(r, g, b)
        => Pixel(r, g, b, 100)
      }

      Image (TileScale, TileScale, tileArray: Array[Pixel] )
  }

  /**
  * function of generateImage signature, which I can use localy to generate Tile images
   */
  def imageGenerator(year:Year, drawingTile:Tile, yearData:Iterable[(Location, Temperature)]):Unit={

    val image:Image=tile(yearData,ColorScale,drawingTile)

    val outputFile = new java.io.File(s"target//temperatures//$year//${drawingTile.zoom}//${drawingTile.x}-${drawingTile.y}.png")
    outputFile.getParentFile.mkdirs()
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
      for (zoom<-(0 to 3); x<-(0 until pow(2.0,zoom.toDouble).toInt); y<-(0 until pow(2.0,zoom.toDouble).toInt) ) {
        generateImage(year,Tile(x,y,zoom),data)
      }
    }
  }

}
