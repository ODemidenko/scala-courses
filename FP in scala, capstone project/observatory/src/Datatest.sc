import java.nio.file.{Files, Paths}
import java.nio.file._
import scala.collection.JavaConversions._

import observatory.Extraction

Extraction.fsPath("\\stations.csv")

Extraction.readStations(Extraction.fsPath("\\stations.csv")).show()

Extraction.readTemperature(Extraction.fsPath("\\1975.csv")).show()
Extraction.dataETL(1975,"\\stations.csv","\\1975.csv").show()

//Extraction.locateTemperatures(1975,"\\stations.csv","\\1975.csv")

