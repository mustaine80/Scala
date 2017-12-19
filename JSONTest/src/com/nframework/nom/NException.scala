package com.nframework.nom


abstract class NException(s: String) extends Throwable {
  def tostring()
  
}

class NInvalidFieldException(s: String) extends NException(s) {
  def tostring() {
    val builder = new StringBuffer
    builder.append("[NOM-InvalidFieldException occurred]: ")
    builder.append(s)
    
    builder.toString()
  }
}

class NArrayIndexOutOfBoundsException(s: String) extends NException(s) {
  def tostring() {
    val builder = new StringBuffer
    builder.append("[NOM-NArrayIndexOutOfBoundsException Occurred]: ")
    builder.append(s)
    
    builder.toString()
  }
}

class NTypeMismatchException(s: String) extends NException(s) {
  def tostring() {
    val builder = new StringBuffer
    builder.append("[NOM-NTypeMismatchException Occurred]: ")
    builder.append(s)
    
    builder.toString()
  }
}

class NNOMFileParsingFailedException(s: String) extends NException(s) {
  def tostring() {
    val builder = new StringBuffer
    builder.append("[NOM-NNOMFileParsingFailedException Occurred]: ")
    builder.append(s)
    
    builder.toString()
  }
}
