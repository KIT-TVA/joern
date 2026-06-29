package io.joern.c2cpg.astcreation.converter

class VAstConverterState(private val patternConverterStates: Map[VAstPatternConverter, Any] = Map.empty) {

  def getState(patternConverter: VAstPatternConverter): Any = patternConverterStates.get(patternConverter).get

  def updateState(patternConverter: VAstPatternConverter, newState: Any): VAstConverterState =
    VAstConverterState(patternConverterStates.updated(patternConverter, newState))

}
