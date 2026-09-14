package io.joern.c2cpg.astcreation.converter

class VAstConverterState(private val patternConverterStates: Map[VAstPatternConverter | VAstHandler, Any] = Map.empty) {

  def getState(patternConverter: VAstPatternConverter | VAstHandler): Any = patternConverterStates(patternConverter)

  def updateState(patternConverter: VAstPatternConverter | VAstHandler, newState: Any): VAstConverterState =
    VAstConverterState(patternConverterStates.updated(patternConverter, newState))

}
