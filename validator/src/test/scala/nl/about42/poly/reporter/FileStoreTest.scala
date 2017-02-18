package nl.about42.poly.reporter

import java.io.File

import org.scalatest.FlatSpec

/**
  * Created by Jean-Marc van Leerdam on 2017-02-18
  */
class FileStoreTest extends FlatSpec {

  "A FileStore" should "accept a valid name" in {
    val myStore = new FileStore(".")

    assert (myStore.getSaneFile("foo.txt").getPath == "./foo.txt")
  }

  it should "not accept an empty name" in {
    val myStore = new FileStore(".")

    assert (myStore.getSaneFile("").getPath == "./status.json")
  }

  it should "use a relative path" in {
    val myStore = new FileStore("./results")
    assert (myStore.getSaneFile("size_5.json").getPath == "./results/size_5.json")
  }

  it should "know a proper root" in {
    val myStore = new FileStore("./frontend/src/main/resources/www/results")
    val myRoot = new File(".").getCanonicalPath
    assert (myStore.getSaneFile("size_5.json").getCanonicalPath == s"$myRoot/frontend/src/main/resources/www/results/size_5.json")

  }

}
