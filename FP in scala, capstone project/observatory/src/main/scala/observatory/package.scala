import java.time.LocalDate

package object observatory {
  type Temperature = Double // °C, introduced in Week 1
  type Year = Int // Calendar year, introduced in Week 1
  type LocatedTemperature = (LocalDate, Location, Temperature)

  val ColorScale: List[(Temperature, Color)] =
    List[(Temperature, Color)](
      (60d,Color(255,255,255)),
      (32d,Color(255,0,0)),
      (12d,Color(255,255,0)),
      (0d,Color(0,255,255)),
      (-15d,Color(0,0,255)),
      (-27d,Color(255,0,255)),
      (-50d,Color(33,0,107)),
      (-60d,Color(0,0,0))
    )
}
