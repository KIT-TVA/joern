package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew

abstract class VAstHandler(vAstCreator: VAstCreatorNew, converter: VAstConverter) {
  
  def getInitialConverterState: Any = null
}
