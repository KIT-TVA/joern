package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew

import scala.collection.mutable

class VAstLogicHandler(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstHandler(vAstCreator, converter) {

  def isTautology(condition: String): Boolean = if (isTrue(condition)) true else {
    isTrue(simplify(condition))
  }

  private def isTrue(condition: String): Boolean = if (condition == null) true else {
    val cleanedCondition: String = condition.replace(" ", "").replace("!!", "")
    condition.equals("") || cleanedCondition.equals("1") || cleanedCondition.equals("!0")
  }

  private def isFalse(condition: String): Boolean = if (condition == null) false else {
    val cleanedCondition: String = condition.replace(" ", "").replace("!!", "")
    cleanedCondition.equals("0") || cleanedCondition.equals("!1")
  }

  def isSatisfiable(condition: String): Boolean = condition match {
    case cond if isTrue(cond) => true
    case cond if isFalse(cond) => false
    case _ =>
      val expression: Seq[Seq[String]] = stringToExpression(condition)
      val normalizedExpression: Seq[Seq[String]] = expression.flatMap(normalizeAndExpression)
      normalizedExpression.nonEmpty
  }

  def combineAndSimplifyConditionsAnd(conditions: Seq[String]): String = {
    conditions.size match {
      case 0 => "1"
      case 1 => simplify(conditions.head)
      case _ =>
      val allExpressions: Seq[Seq[Seq[String]]] = conditions.map ((condition: String) => stringToExpression(condition))
      var combinedAndSimplifiedExpression: Seq[Seq[String]] = allExpressions.head
      for (nextExpression: Seq[Seq[String]] <- allExpressions.tail) {
        combinedAndSimplifiedExpression = combineAndSimplifyTwoExpressionsAnd(combinedAndSimplifiedExpression, nextExpression)
      }
      expressionToString(combinedAndSimplifiedExpression)
    }
  }

  private def combineAndSimplifyTwoExpressionsAnd(firstExpression: Seq[Seq[String]],
                                                secondExpression: Seq[Seq[String]]): Seq[Seq[String]] = {
    // Combines the conditions.
    val combinedExpression: Seq[Seq[String]] = firstExpression.flatMap((firstAndExpression: Seq[String]) => {
      secondExpression.map((secondAndExpression: Seq[String]) => firstAndExpression ++ secondAndExpression)
    })
    
    // Simplifies the combined expression.
    simplify(combinedExpression)
  }

  def combineAndSimplifyConditionsOr(conditions: Seq[String]): String = {
    // Checks if one of the passed conditions a tautology is.
    if (conditions.exists((condition: String) => isTautology(condition))) {
      // If one of the conditions is a tautology.
      "1"

    } else {
      // If none of the conditions is a tautology.
      val combinedExpressions: Seq[Seq[String]] = conditions.flatMap((condition: String) => stringToExpression(condition))
      val simplifiedExpression: Seq[Seq[String]] = simplify(combinedExpressions)
      expressionToString(simplifiedExpression)
    }
  }
  
  private def stringToExpression(condition: String) : Seq[Seq[String]] = {
    if (isTrue(condition)) Seq(Seq("1")) else {
      condition.split(" \\|\\| ").map((andTerm: String) => {
        if (isTrue(andTerm)) Seq("1") else andTerm.split(" && ").toSeq
      }).filter((andExpression: Seq[String]) => andExpression.nonEmpty).toSeq
    }
  }

  private def expressionToString(expression: Seq[Seq[String]]): String = {
    val andTerms: Seq[String] = expression.map((andExpression: Seq[String]) => {
      val cleanedAndExpressions: Seq[String] =
        andExpression.filterNot((variableExpression: String) => isTrue(variableExpression)).sorted
      if (cleanedAndExpressions.isEmpty ) "1" else cleanedAndExpressions.mkString(" && ")
    })
    
    if (andTerms.exists((andTerm: String) => isTrue(andTerm))) "1" else  andTerms.sorted.mkString(" || ")
  }
  
  def equivalent(firstCondition: String, secondCondition: String): Boolean = {
    val simplifiedFirstCondition: String = simplify(firstCondition)
    val simplifiedSecondCondition: String = simplify(secondCondition)
    
    // Checks if the conditions are equivalent.
    simplifiedFirstCondition.equals(simplifiedSecondCondition)
  }
  
  private def simplify(condition: String): String = {
    val expression: Seq[Seq[String]] = stringToExpression(condition)
    val simplifiedExpression: Seq[Seq[String]] = simplify(expression)
    expressionToString(simplifiedExpression)
  }

  private def simplify(expression: Seq[Seq[String]]): Seq[Seq[String]] = {
    // Normalizes and remove unsatisfiable expressions parts.
    val normalizedExpression: Seq[Seq[String]] = expression.flatMap(normalizeAndExpression)

    normalizedExpression match {
      case exp if exp.isEmpty => Seq(Seq("0")) // If the passed expression is not satisfiable.
      case exp if exp.exists((andExp: Seq[String]) => (andExp.size == 1) && isTrue(andExp.head)) => Seq(Seq("1")) // If the passed expression is a tautology.
      case exp =>
        // If the passed expression is satisfiable and contains no tautology and-expressions. This does not mean that
        // the passed expression itself is not a tautology.
        getPrimeImplicants(exp)
    }
  }

  /**
   * Removes unnecessary parts of the and-expression and returns the nomelized/simplified and-expression.
   * 
   * @param andExpression The and-expression to be normalized/simplified. 
   * @return Returns the nomelized/simplified and-expression as an option. If the Option is None, the passed
   *         and-expression is not satisfiable.
   */
  private def normalizeAndExpression(andExpression: Seq[String]): Option[Seq[String]] = {
    val literals: Seq[String] = andExpression.distinct
    val variable_names: Seq[String] = literals.map(removeNegation)

    // TODO: The following check could also verify the internal literals more thoroughly for feasibility (use a switch-case statement).
    //  (M0 > 0) && (M0 == 0) => not-satisfiable
    //  (M0 >= 0) && (M0 == 0) => (M0 == 0)
    // Checks whether the expression is satisfiable.
    if ((variable_names.exists((name: String) => literals.contains(name) && literals.contains("!" + name)))
      || literals.exists((literal: String) => isFalse(literal))) None else {
      // If the and-expression is satisfiable (based on the limited check).
      val normalizedLiterals: Seq[String] = literals.filterNot((literal: String) => isTrue(literal))
      if (normalizedLiterals.isEmpty) Some(Seq("1")) else Some(normalizedLiterals)
    }
  }

  private def negate(literal: String): String = if (literal.startsWith("!")) literal.drop(1) else "!" + literal

  private def removeNegation(literal: String): String = if (literal.startsWith("!")) literal.drop(1) else literal

  /**
   * Calculates the prim implicants with the Quine McCluskey algorithm.
   * 
   * @param dfExpression
   * @return
   */
  private def getPrimeImplicants(dfExpression: Seq[Seq[String]]): Seq[Seq[String]] = {
    if (dfExpression.size <= 1) {
      // No simplification is requiered/possible
      dfExpression

    } else {
      // Simplify the expression with the Quine McCluskey algorithm.

      // Creates the map to convert the variable names to indices.
      val variableNames: List[String] = dfExpression
        .flatMap((andExpression: Seq[String]) => andExpression.map((variable: String) => removeNegation(variable)))
        .toList.distinct
      val variableNameMap: Map[String, Int] = variableNames.zipWithIndex.toMap // Variable name to variable index map
      val numberOfVariableNames: Int = variableNames.size

      // Converts the DF expressions to logic strings made of '0', '1' and '-' (0 and 1 at the same time)
      val allTrue: String = "-" * numberOfVariableNames
      val logicStrings: List[String] = dfExpression.map((andExpression: Seq[String]) => {
        var logicString: String = allTrue
        for (variable <- andExpression) {
          // Modifies the logic string of the and-expression.
          val variableName = removeNegation(variable)
          logicString = logicString.updated(variableNameMap(variableName), if (variable.startsWith("!")) '0' else '1')
        }
        logicString
      }).toList

      // Groups the logic string base on the number of wildcards ('-') and contained '1' (required for Quine McCluskey algorithm).
      // Type: Map[<number of wildcards '-'>, Map[<number of contained '1'>, Map[<logic string>, Seq[<logic string IDs covered by expression>]]]
      val logicStringsGroupedByWildcards: mutable.Map[Int, mutable.Map[Int, mutable.Map[String, Set[Int]]]] = mutable.Map.from(logicStrings
        .zipWithIndex // List[(String, Int)] | Assigns all logic strings a unique ID (the index of the logic string).
        .map((logicString, refIndices) => (logicString, Set(refIndices))) // List[(String, Set[Int])] | Prepares the logic string ID referencing for the Quine McCluskey algorithm.
        .groupBy((logicString, refIndices) => logicString.count(_ == '-')) // Map[Int, List[(String, Set[Int])]] | Groups logic strings by number of wildcards '-' (optimization for Quine McCluskey algorithm).
        .map((wildcards: Int, logicStringGroup: List[(String, Set[Int])]) => { // Map[Int, Map[Int, Map[String, Set[Int]]]] | Finalizes the initial data  structure for the Quine McCluskey algorithm.
          val logicStringGroupByNumberOfOnes: mutable.Map[Int, mutable.Map[String, Set[Int]]] = mutable.Map.from(logicStringGroup
            .groupBy((logicString: String, refIndices: Set[Int]) => logicString.count(_ == '1')) // Map[Int, List[(String, Set[Int]] | Groups logic string in each wildcard groups by the number of contain ones (required for Quine McCluskey algorithm).
            .map((ones: Int, expression: List[(String, Set[Int])]) => (ones, mutable.Map.from(expression))))
          (wildcards, logicStringGroupByNumberOfOnes)
        }))

      // Calculates the prime implicants.
      var wildcards: Int = 0
      val nonPrimeImplicantsLogicStrings: mutable.Set[String] = mutable.Set.empty[String] // Requiered for nonPrimImplicant detection.
      val primeImplicantsLogicStrings: mutable.Map[String, Set[Int]] = mutable.Map.empty[String, Set[Int]] // Type: Mpa[<logic string>, Seq[<logic string IDs covered by expression>]]
      for (wildcardIndex: Int <- 0 until numberOfVariableNames) {
        val currentLogicStringsGroupedByOnes: mutable.Map[Int, mutable.Map[String, Set[Int]]] = // Type: Map[<number of contained '1'>, Map[<logic string>, Seq[<logic string IDs covered by expression>]]
          logicStringsGroupedByWildcards.getOrElse(wildcardIndex, mutable.Map.empty[Int, mutable.Map[String, Set[Int]]])
        wildcards = wildcardIndex + 1

        currentLogicStringsGroupedByOnes.size match {
          case 0 =>
            // No logic expressions defined => nothing to do
            
          case 1 => 
            // All logic strings are prime implicants because they are never used for a reduction.
            currentLogicStringsGroupedByOnes(currentLogicStringsGroupedByOnes.keys.head)
              .foreach((logicString: String, refIndices: Set[Int]) => {
                if (primeImplicantsLogicStrings.contains(logicString)) {
                  // If the Logic string already exist
                  primeImplicantsLogicStrings(logicString) ++= refIndices
                } else {
                  primeImplicantsLogicStrings.addOne((logicString, refIndices))
                }
              })
            
          case _ =>
            // Expressions have to be checked for possible simplifications (removability of one variable).
            val nextLogicStringsGroupedByOnes: mutable.Map[Int, mutable.Map[String, Set[Int]]] = if (logicStringsGroupedByWildcards.contains(wildcards)) {
              // Returns the map with the known logic strings with the requested number of wildcards.
              logicStringsGroupedByWildcards(wildcards)
            } else {
              // If no logi string with the request number of wildcard are known.
              mutable.Map.empty[Int, mutable.Map[String, Set[Int]]]
            }

            // The logic strings have to be compared pairwise inorder to simplify the logic expression.
            val allOneGroupIndices: Seq[Int] = currentLogicStringsGroupedByOnes.keys.toSeq.sorted
            for (lowerOneGroupIndex: Int <- allOneGroupIndices) {
              if (allOneGroupIndices.contains(lowerOneGroupIndex + 1)) {
                // If the comparison of the two one logic string groups is necessary (at least one logic string with one more '1' exists in the current wildcard group).
                // Prepares everything for the simplification of the one logic string group.
                val lowerLogicStringGroup: mutable.Map[String, Set[Int]] = currentLogicStringsGroupedByOnes(lowerOneGroupIndex)
                val upperLogicStringGroup: mutable.Map[String, Set[Int]] = currentLogicStringsGroupedByOnes(lowerOneGroupIndex + 1)

                // Compare logic strings pairwise inorder to simplify the logic expression and determine the prime implicants.
                for ((logicString: String, refIndices: Set[Int]) <- lowerLogicStringGroup) {
                  // Checks all possible logic string simplifications for the current logic string
                  for (variableNameIndex <- 0 until numberOfVariableNames) {
                    if (logicString.charAt(variableNameIndex) == '0') {
                      // If the current variable index could be relevant for a simplification.

                      // Checks if the twin logic string exist with a one at the position variableNameIndex and retrieves the ref indices.
                      val twinLogicString: String = logicString.patch(variableNameIndex, "1", 1)
                      val otherRefIndices: Set[Int] = upperLogicStringGroup
                        .filter((otherLogString: String, _: Set[Int]) => twinLogicString.equals(otherLogString))
                        .flatMap((_: String, otherRefIndices: Set[Int]) => otherRefIndices).toSet

                      if (otherRefIndices.nonEmpty) {
                        // If the twin logic string exist.
                        // Saves the new/reduced simplified logic string.
                        val newLogString: String = logicString.patch(variableNameIndex, "-", 1) // The simplified logic string
                        val newRefIndices: Set[Int] = refIndices ++ otherRefIndices // The logic string ID of the covered logic strings.
                        if (nextLogicStringsGroupedByOnes.contains(lowerOneGroupIndex)) {
                          // If the one logic string group of the combined logic string already exists.
                          // Adds or extends the enty for combined logic string.
                          val currentLogicStringsGroupedByOne: mutable.Map[String, Set[Int]] = nextLogicStringsGroupedByOnes(lowerOneGroupIndex)
                          val currentRefIndices: Set[Int] = currentLogicStringsGroupedByOne.getOrElse(newLogString, Set.empty[Int])
                          currentLogicStringsGroupedByOne(newLogString) = currentRefIndices ++ newRefIndices
                        } else {
                          // If the one logic string group of the combined logic string not exists.
                          // Creates the required one logic string group and adds the combined logic string to this group.
                          nextLogicStringsGroupedByOnes.addOne((lowerOneGroupIndex, mutable.Map(newLogString -> newRefIndices)))
                        }

                        // Marks the logic string pair as non-prime implicants.
                        nonPrimeImplicantsLogicStrings.add(logicString)
                        nonPrimeImplicantsLogicStrings.add(twinLogicString)
                      }
                    }
                  }

                  // Adds the lower logic string to the prime implicants if it was not used in a reduction.
                  if (!nonPrimeImplicantsLogicStrings.contains(logicString)) {
                    primeImplicantsLogicStrings.addOne((logicString, refIndices))
                  }
                }

              } else {
                // If no further simplification comparisons for the current one logic string groups are required.
                // Adds the logic strings to the prime implicants if they were not used in a reduction.
                val newPrimeImplicantsLogicStrings: mutable.Map[String, Set[Int]] = currentLogicStringsGroupedByOnes(lowerOneGroupIndex)
                  .filterNot((logicString: String, _: Set[Int]) => nonPrimeImplicantsLogicStrings.contains(logicString))
                for (logicExpression: (String, Set[Int]) <- newPrimeImplicantsLogicStrings) {
                  primeImplicantsLogicStrings.addOne(logicExpression)
                }
              }
            }
            if (nextLogicStringsGroupedByOnes.nonEmpty) {
              // Updates the logic string wildcard data structure.
              logicStringsGroupedByWildcards.addOne((wildcards, nextLogicStringsGroupedByOnes))
            }
        }
      }

      // Removes all prime implicant that are covered by other prime implicants.
      val relevantPrimeImplicants: Seq[(String, Set[Int])] = primeImplicantsLogicStrings.toSeq
        .filterNot((logicString: String, refIndices: Set[Int]) => { // Delete all prime implicants that are covered by others.
          primeImplicantsLogicStrings.exists((otherLogicString: String, otherRefIndices: Set[Int]) =>
            (logicString != otherLogicString) && refIndices.subsetOf(otherRefIndices))
        })
      
      // TODO: Needs to be checked for completeness.
      // Finds the minimal prime implicant combination.
      val requiredPrimeImplicants: Seq[(String, Set[Int])] = relevantPrimeImplicants
        .filter((logicString: String, refIndices: Set[Int]) => { // Determine all prime implicants that are the only ones covering an expression, since these are in any case part of the minimal prime implicants.
          var notCoveredRefIndices: Set[Int] = refIndices
          for ((otherLogicString: String, otherRefIndices: Set[Int]) <- relevantPrimeImplicants) {
            if (!otherLogicString.equals(logicString)) {
              notCoveredRefIndices = notCoveredRefIndices.diff(otherRefIndices)
            }
          }
          notCoveredRefIndices.nonEmpty
        })
      val coveredRefIndices: Set[Int] = requiredPrimeImplicants.flatMap((l: String, refIndices: Set[Int]) => refIndices).toSet
      val minimalPrimeImplicants: Seq[String] = relevantPrimeImplicants.filterNot((logicString: String, refIndices: Set[Int]) => {
        !requiredPrimeImplicants.exists((otherLogicString: String, _: Set[Int]) => otherLogicString.equals(logicString)) // Checks if the logic string is not a required prime implicant.
          && refIndices.subsetOf(coveredRefIndices) // Checks if the logic string is already covered.
      }).map((logicString: String, refIndices: Set[Int]) => logicString)

      // Reconstruct logical expression from the minimal prime implicant logic strings.
      minimalPrimeImplicants.map((logicString: String) => {
        val andExpression: Seq[String] = logicString.toList.zipWithIndex
          .filterNot((alg: Char, variableNameIndex: Int) => alg == '-')
          .map((alg: Char, variableNameIndex: Int) => {
            val variableName: String = variableNames(variableNameIndex)
            if ((alg == '0') == variableName.startsWith("!")) variableName else negate(variableName)
          })
        
        // Special treatment of “true”.
        if (andExpression.isEmpty) Seq("1") else andExpression
      })
    }
  }
}
