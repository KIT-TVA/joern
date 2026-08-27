package io.joern.c2cpg.astcreation

import io.joern.c2cpg.astcreation.converter.{VASTPatternConverterForEmptyDefinition, VAstConverter, VAstPatternConverter, VAstPatternConverterForBinaryOperators, VAstPatternConverterForBreakContinue, VAstPatternConverterForCast, VAstPatternConverterForConditionalMacro, VAstPatternConverterForForLoop, VAstPatternConverterForFunctionCall, VAstPatternConverterForFunctionDeclaration, VAstPatternConverterForIf, VAstPatternConverterForMemberAccess, VAstPatternConverterForParenthesizedExpression, VAstPatternConverterForSuperCRoot, VAstPatternConverterForUnaryOperators, VAstPatternConverterForVariableDeclaration, VAstPatternConverterForWhileLoop}

class VAstConverterForC(private var vAstCreator: VAstCreatorNew) extends VAstConverter(vAstCreator) {
  private val conditionalConverter = new VAstPatternConverterForConditionalMacro(vAstCreator, this)
  private val patterns: List[VAstPatternConverter] = List.apply(
    new VAstPatternConverterForBinaryOperators(vAstCreator, this),
    new VAstPatternConverterForBreakContinue(vAstCreator, this),
    new VAstPatternConverterForCast(vAstCreator, this),
    conditionalConverter,
    new VAstPatternConverterForFunctionCall(vAstCreator, this),
    new VAstPatternConverterForMemberAccess(vAstCreator, this),
    new VAstPatternConverterForParenthesizedExpression(vAstCreator, this),
    new VAstPatternConverterForUnaryOperators(vAstCreator, this),
    new VAstPatternConverterForFunctionDeclaration(vAstCreator, this),
    new VAstPatternConverterForSuperCRoot(vAstCreator, this),
    new VAstPatternConverterForVariableDeclaration(vAstCreator, this),
    // For before While: both register IterationStatement; first matching converter wins.
    new VAstPatternConverterForForLoop(vAstCreator, this),
    new VAstPatternConverterForIf(vAstCreator, this),
    new VAstPatternConverterForWhileLoop(vAstCreator, this),
    new VASTPatternConverterForEmptyDefinition(vAstCreator, this)
  )
  super.addPatterns(patterns)
  super.addConditionalHandler(conditionalConverter)
}
