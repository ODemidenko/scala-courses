package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math.pow

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    d00*(1-point.x)*(1-point.y)+d10*point.x*(1-point.y)+d01*(1-point.x)*point.y+d11*point.x*point.y
  }


  def getGridCoordinates(location: Location)={
    val latRemainder=if (location.lat%GridPrecision==0) GridPrecision else location.lat%GridPrecision
    val latLower=if ((location.lat-latRemainder)< -90) location.lat else (location.lat-latRemainder)
    val latHigher=latLower+GridPrecision

    val lonRemainder=if (location.lon%GridPrecision==0) GridPrecision else location.lon%GridPrecision
    val lonLower=if ((location.lon-lonRemainder)< -180) location.lon else (location.lon-lonRemainder)
    val lonHigher=lonLower+GridPrecision
    ((
      GridLocation(latLower.toInt,lonLower.toInt),
      GridLocation(latLower.toInt,lonHigher.toInt),
      GridLocation(latHigher.toInt,lonLower.toInt),
      GridLocation(latHigher.toInt,lonHigher.toInt)
    ),
    CellPoint(lonRemainder,latRemainder)
    )
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {

      val coordinates=Interaction.tileCoordinatesArray(tile)
      Image(TileScale,TileScale,
          for(interGridPoint<-coordinates) yield {
            val (coords,cellPoint)=getGridCoordinates(interGridPoint)
            val temp=bilinearInterpolation(cellPoint,
              grid(coords._1),
              grid(coords._2),
              grid(coords._3),
              grid(coords._4)
            )
            Visualization.interpolateColor(colors,temp)  match
            {case Color(r,g,b)=>Pixel(r,g,b,100)}
          }
      )
    }

  def deviationsImageGenerator(year:Year, tile:Tile, gridOfDeviations:GridLocation => Temperature ):Unit={

    val outputFile = new java.io.File(s"target//temperatures//$year//${tile.zoom}//${tile.x}-${tile.y}.png")
    outputFile.getParentFile.mkdirs()

    val image=visualizeGrid(gridOfDeviations,ColorDeviations,tile)
    image.output(outputFile)
  }

  def deviationTilesGenerator(yearFrom:Year,yearTo:Year):Unit={
    val annualAvgs=for (year<-1975 to 1989) yield
      Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(year,"\\stations.csv",s"\\$year.csv"))

    //todo: (second priority) try to parallelize it. in order ot be parallelizible - should switch from foldLeft to smth associative
    // ! It makes sense just to implement an alternative, calculating normals and yearly deviations, in spark!!
    // separate methods could be implemented through spark, as an alternative.
    // execution mode (spark/standalone) - can be determined by an implicit param. with "core scala" default value
    val normals=Manipulation.average(annualAvgs)

    //    Compute deviations for years between 1990 and 2015 ;
    for(year<-yearFrom to yearTo) {
      val yearlyData=Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(year,"\\stations.csv",s"\\$year.csv"))
      //todo: (second priority) try to parallelize it
      val gridOfDeviations=Manipulation.deviation(yearlyData, normals)

      //Generate tiles for zoom levels going from 0 to 3, showing the deviations.
      //todo: parallize image creation? Am I right that it is enough to .par zoom collection? Bechmark it!
      //Implementing this on spark - put tiles into Spark and
      for (zoom<-(0 to 3).par; x<-(0 until pow(2.0,zoom.toDouble).toInt); y<-(0 until pow(2.0,zoom.toDouble).toInt) ) {
        //todo: here every grid location will be reused multiple times for different locations and tile scales
        val image=Visualization2.visualizeGrid(gridOfDeviations,ColorDeviations,Tile(x,y,zoom))

        //Use the output method of Image to write the tiles on your file system, under a location named according to the following scheme:
        // target/deviations/<year>/<zoom>/<x>-<y>.png.
        val outputFile = new java.io.File(s"target//deviations//$year//${zoom}//${x}-${y}.png")
        outputFile.getParentFile.mkdirs()
        image.output(outputFile)
      }

    }
  }
}
