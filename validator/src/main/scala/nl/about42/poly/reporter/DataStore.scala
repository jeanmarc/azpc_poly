package nl.about42.poly.reporter

import java.io.{File, PrintWriter}

/**
  * Data Store interface
  */
class DataStore {
  def save(name: String, data: String) = {}
}

class ConsoleStore extends DataStore {
  override def save(name: String, data: String): Unit = {
    System.out.println(s"$name result is $data")
  }
}

/**
  * File Store, will save to a file root
  * @param root
  */
class FileStore( root: String) extends DataStore {
  override def save(name: String, data: String): Unit = {
    assert(name != "")
    val writer = new PrintWriter(getSaneFile(name))
    writer.write(data)
    writer.close()
  }

  def getSaneFile( name: String): File = {
    return name match {
      case "" => new File(root, "status.json")
      case x => new File(root, x)
    }
  }
}
