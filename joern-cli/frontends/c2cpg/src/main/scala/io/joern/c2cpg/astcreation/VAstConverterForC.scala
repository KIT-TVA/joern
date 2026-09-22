package io.joern.c2cpg.astcreation

import io.joern.c2cpg.astcreation.converter.{VASTPatternConverterForEmptyDefinition, VAstConditionalHandler, VAstConverter, VAstVariableHandler, VAstLogicHandler, VAstPatternConverter, VAstPatternConverterForBinaryOperators, VAstPatternConverterForBreakContinue, VAstPatternConverterForCast, VAstPatternConverterForConditionalMacro, VAstPatternConverterForEnum, VAstPatternConverterForForLoop, VAstPatternConverterForFunctionCall, VAstPatternConverterForFunctionDeclaration, VAstPatternConverterForGoto, VAstPatternConverterForIf, VAstPatternConverterForMemberAccess, VAstPatternConverterForParenthesizedExpression, VAstPatternConverterForReturn, VAstPatternConverterForSuperCRoot, VAstPatternConverterForSwitch, VAstPatternConverterForUnaryOperators, VAstPatternConverterForVariableDeclaration, VAstPatternConverterForWhileLoop}

class VAstConverterForC(private var vAstCreator: VAstCreatorNew) extends VAstConverter(vAstCreator) {
  super.addLogicHandler(new VAstLogicHandler(vAstCreator, this))
  super.addConditionalHandler(new VAstConditionalHandler(vAstCreator, this))
  super.addDeclarationHandler(new VAstVariableHandler(vAstCreator, this))

  private val patterns: List[VAstPatternConverter] = List.apply(
    new VAstPatternConverterForBinaryOperators(vAstCreator, this),
    new VAstPatternConverterForBreakContinue(vAstCreator, this),
    new VAstPatternConverterForCast(vAstCreator, this),
    new VAstPatternConverterForConditionalMacro(vAstCreator, this),
    new VAstPatternConverterForEnum(vAstCreator, this),
    new VAstPatternConverterForFunctionCall(vAstCreator, this),
    new VAstPatternConverterForGoto(vAstCreator, this),
    new VAstPatternConverterForMemberAccess(vAstCreator, this),
    new VAstPatternConverterForParenthesizedExpression(vAstCreator, this),
    new VAstPatternConverterForUnaryOperators(vAstCreator, this),
    new VAstPatternConverterForFunctionDeclaration(vAstCreator, this),
    new VAstPatternConverterForReturn(vAstCreator, this),
    new VAstPatternConverterForSuperCRoot(vAstCreator, this),
    new VAstPatternConverterForVariableDeclaration(vAstCreator, this),
    // For before While: both register IterationStatement; first matching converter wins.
    new VAstPatternConverterForForLoop(vAstCreator, this),
    new VAstPatternConverterForIf(vAstCreator, this),
    // Switch after If: both register SelectionStatement; If returns None for "switch".
    new VAstPatternConverterForSwitch(vAstCreator, this),
    new VAstPatternConverterForWhileLoop(vAstCreator, this),
    new VASTPatternConverterForEmptyDefinition(vAstCreator, this)
  )
  super.addPatterns(patterns)
}
