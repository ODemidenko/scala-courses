import java.time.LocalDate

package object observatory {
  type Temperature = Double // °C, introduced in Week 1
  type Year = Int // Calendar year, introduced in Week 1
  type LocatedTemperature = (LocalDate, Location, Temperature)
}
