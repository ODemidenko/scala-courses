import java.nio.file.{Files, Paths}
import java.nio.file._
import scala.collection.JavaConversions._

import observatory.Extraction


getClass.getResource("\\stations.csv").getPath
Paths.get(getClass.getResource("\\stations.csv").toURI).toString
//т.  е. Linux-style path не воспринимается


//Extraction.readStations(Extraction.fsPath("\\stations.csv")).show()
Extraction.readStations(Extraction.fsPath("\\stations.csv")).show()
//
//Extraction.readTemperature(Extraction.fsPath("\\1975.csv")).show()
////Extraction.dataETL(1975,"\\stations.csv","\\1975.csv").show()
//val records=Extraction.locateTemperatures(1975,"\\stations.csv","\\1975.csv")
//
//  records.head
//
//Extraction.sparkAverageRecords(records).take(2)





