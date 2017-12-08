package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    val grid =(
      for(lat<-(-90 to 90 by GridPrecision).par; lon<-(-180 to 180 by GridPrecision))  yield
        GridLocation(lat,lon) -> Visualization.predictTemperature(temperatures,Location(lat,lon))
      ).toMap

    gridLocation=>grid(gridLocation)
    /*
    alternative without memoization
    case GridLocation(lat,lon)=>Visualization.predictTemperature(temperatures,Location(lat,lon))
     */
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    /*
    To avert whole collection scanning for every grid location - it makes sense to construct some interim collection,
    decreasing number of pathes through the data.
    As we have outer iterator by data - it makes sense to calculate each year's grid temperatures and store them as
    Map[GridLocation,Iterable[Temperature]]. This might be obtained in a single pass through years, allowing better data
    locality(?),  meaning that same annual data is used to calculate all data points.
    Without memoization- we are passing through the data as many times, as many locations we serve. But no memory overhead
    and simple coder
     */

    val averageGrids ={
      val annualGrids=temperaturess.map(year_temps=>makeGrid(year_temps))
        (
      for(lat<-(-90 to 90 by GridPrecision).par; lon<-(-180 to 180 by GridPrecision)) yield
        GridLocation(lat,lon) ->
          annualGrids.map(grid=>grid(GridLocation(lat,lon)))
          .foldLeft(0:Temperature,0:Int){case ((avg,count),temp)=>((avg*count+temp)/(count+1),count+1)}._1
          ).toMap
    }
    gridLocation=>averageGrids(gridLocation)
    /*
    alternative without memoization
    gridLocation=> temperaturess.map(year_temps=>makeGrid(year_temps)(gridLocation))
      .foldLeft(0:Temperature,0:Int){case ((avg,count),temp)=>((avg*count+temp)/(count+1),count+1)}._1
      */
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    //todo: as this grid will be reused multiple times - its values should be memoised
    val memoized=makeGrid(temperatures)
    gridLocation=>memoized(gridLocation)-normals(gridLocation)
  }


}

