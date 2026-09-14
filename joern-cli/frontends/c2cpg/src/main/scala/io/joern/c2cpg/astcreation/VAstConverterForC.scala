package io.joern.c2cpg.astcreation

import io.joern.c2cpg.astcreation.converter.{VASTPatternConverterForEmptyDefinition, VAstConditionalHandler, VAstConverter, VAstDeclarationHandler, VAstLogicHandler, VAstPatternConverter, VAstPatternConverterForBinaryOperators, VAstPatternConverterForCast, VAstPatternConverterForConditionalMacro, VAstPatternConverterForFunctionCall, VAstPatternConverterForFunctionDeclaration, VAstPatternConverterForMemberAccess, VAstPatternConverterForParenthesizedExpression, VAstPatternConverterForReturn, VAstPatternConverterForSuperCRoot, VAstPatternConverterForUnaryOperators, VAstPatternConverterForVariableDeclaration, VAstPatternConverterForWhileLoop}

class VAstConverterForC(private var vAstCreator: VAstCreatorNew) extends VAstConverter(vAstCreator) {
  super.addLogicHandler(new VAstLogicHandler(vAstCreator, this))
  super.addConditionalHandler(new VAstConditionalHandler(vAstCreator, this))
  super.addDeclarationHandler(new VAstDeclarationHandler(vAstCreator, this))

  private val patterns: List[VAstPatternConverter] = List.apply(
    new VAstPatternConverterForBinaryOperators(vAstCreator, this),
    new VAstPatternConverterForCast(vAstCreator, this),
    new VAstPatternConverterForConditionalMacro(vAstCreator, this),
    new VAstPatternConverterForFunctionCall(vAstCreator, this),
    new VAstPatternConverterForMemberAccess(vAstCreator, this),
    new VAstPatternConverterForParenthesizedExpression(vAstCreator, this),
    new VAstPatternConverterForUnaryOperators(vAstCreator, this),
    new VAstPatternConverterForFunctionDeclaration(vAstCreator, this),
    new VAstPatternConverterForReturn(vAstCreator, this),
    new VAstPatternConverterForSuperCRoot(vAstCreator, this),
    new VAstPatternConverterForVariableDeclaration(vAstCreator, this),
    new VAstPatternConverterForWhileLoop(vAstCreator, this),
    new VASTPatternConverterForEmptyDefinition(vAstCreator, this)
  )
  super.addPatterns(patterns)
}
