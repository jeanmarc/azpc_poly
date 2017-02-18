package nl.about42.poly

import org.scalatest.FlatSpec

/**
  * Created by Jean-Marc van Leerdam on 2017-02-18
  */
class StateBuilderTest extends FlatSpec {

  behavior of "StateBuilderTest"

  it should "fromFile" in {
    StateBuilder.fromFile("frontend/src/main/resources/www/results/status_11.json")
  }

}
