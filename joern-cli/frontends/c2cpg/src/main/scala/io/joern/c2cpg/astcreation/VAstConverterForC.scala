package io.joern.c2cpg.astcreation

import io.joern.c2cpg.astcreation.converter.{VAstConverter, VAstPatternConverter, VAstPatternConverterForBinaryOperators, VAstPatternConverterForConditionalMacro, VAstPatternConverterForFunctionDeclaration, VAstPatternConverterForSuperCRoot, VAstPatternConverterForVariableDeclaration}

class VAstConverterForC(private var vAstCreator: VAstCreatorNew) extends VAstConverter {
  private val patterns: List[VAstPatternConverter] = List.apply(
    new VAstPatternConverterForBinaryOperators(vAstCreator, this),
    new VAstPatternConverterForConditionalMacro(vAstCreator, this),
    new VAstPatternConverterForFunctionDeclaration(vAstCreator, this),
    new VAstPatternConverterForSuperCRoot(vAstCreator, this),
    new VAstPatternConverterForVariableDeclaration(vAstCreator, this)
  )
  super.addPatterns(patterns)
}
