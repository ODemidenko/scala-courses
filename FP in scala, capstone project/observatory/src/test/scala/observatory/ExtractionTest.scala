package observatory

import java.nio.file.Paths

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.collection.JavaConversions.getClass

//trait ExtractionTest extends FunSuite {
class ExtractionTest extends FunSuite {

  test("fsPath comparison to same code results in Scala worksheet") {
    assert(Extraction.fsPath("\\stations.csv") == "S:\\IdeaWorkspace\\Scala\\FP in scala, capstone project\\observatory\\target\\scala-2.11\\classes\\stations.csv",
      "tests still obtain results differently from Scala worksheet")
  }

//  test("fsPath in Class works same way as from the test") {
//      assert(Extraction.fsPath("\\stations.csv") == Paths.get(getClass.getResource("\\stations.csv").toURI).toString,
//        "tests still obtain results differently from Scala worksheet")
//    }
  
}