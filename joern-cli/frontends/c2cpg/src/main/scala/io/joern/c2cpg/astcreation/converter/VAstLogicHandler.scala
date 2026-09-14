package io.joern.c2cpg.astcreation.converter

import io.joern.c2cpg.astcreation.VAstCreatorNew

import scala.collection.mutable

class VAstLogicHandler(vAstCreator: VAstCreatorNew, converter: VAstConverter)
  extends VAstHandler(vAstCreator, converter) {

  def isNoCondition(condition: String): Boolean = (condition == null) || condition.equals("") || condition.equals("1")
  
  def combineAndSimplyConditions(conditions: Seq[String]): String = {
    conditions.size match {
      case 0 => ""
      case 1 => conditions.head
      case _ =>
        var combinedConditions: String = conditions.head
        for (condition <- conditions.tail) {
          combinedConditions = combineAndSimplyTwoConditions(combinedConditions, condition, condition == conditions.last)
        }
        val finalC: String = combinedConditions.split(" \\|\\| ").distinct.mkString(" || ")
        val initial: String = conditions.mkString(") && (")
        finalC
    }
  }

  private def combineAndSimplyTwoConditions(firstConditions: String, secondCondition: String,
                                            fullSimplification: Boolean = false): String = {
    // Combines the two conditions in disjunctive form.
    val firstConditionsParts: Seq[String] = firstConditions.split(" \\|\\| ")
    val secondConditionParts: Seq[String] = secondCondition.split(" \\|\\| ")
    val combinedTerm: Seq[Seq[String]] = firstConditionsParts.flatMap((firstPart: String) => {
      val firstPartSubParts: Seq[String] = firstPart.split(" && ")
      secondConditionParts.map((secondPart: String) => {
        (firstPartSubParts ++ secondPart.split(" && ")).distinct.sorted
      })
    })

    // Simplifies the created new expression.
    var simplifiedCombinedTerm: Seq[Seq[String]] = simplify(combinedTerm)
    if (fullSimplification) {
      simplifiedCombinedTerm = getPrimeImplicants(simplifiedCombinedTerm)
    }

    simplifiedCombinedTerm.map((innerPart: Seq[String]) => innerPart.mkString(" && ")).mkString(" || ")
  }

  def simplify(exp: Seq[Seq[String]]): Seq[Seq[String]] = {
    var terms: Set[Set[String]] = exp.flatMap(normalizeTerm)
      .map((term: Seq[String]) => term.toSet.filterNot((variable: String) => variable.equals("1")))
      .toSet

    var changed = true
    while (changed) {
      val before = terms

      // Remove terms that are covered by shorter terms.
      terms = removeAbsorbedTerms(terms)

      // Combine terms, e.g.: (a && b) || (a && !b)  =>  (a)
      val combinedTerms = for {
        a <- terms
        b <- terms
        combined <- combine(a, b)
      } yield combined

      // (a && b) || (a) => (a)
      terms = removeAbsorbedTerms(terms ++ combinedTerms)
      changed = terms != before
    }

    terms.toList.sortBy(term => (term.size, term.toList.sorted.mkString(","))).map(_.toList.sorted)
  }

  private def normalizeTerm(term: Seq[String]): Option[Seq[String]] = {
    val literals: Seq[String] = term.distinct
    val variable_names: Seq[String] = literals.map(removeNegation)

    // Checks whether the expression is satisfiable.
    if (variable_names.exists((name: String) => literals.contains(name) && literals.contains("!" + name))) None
    else Some(literals)
  }

  private def removeAbsorbedTerms(terms: Set[Set[String]]): Set[Set[String]] = {
    terms.filterNot((term: Set[String]) => terms.exists((other: Set[String]) => (other != term) && other.subsetOf(term)))
  }

  private def combine(a: Set[String], b: Set[String]): Option[Set[String]] = {
    val onlyA = a -- b
    val onlyB = b -- a

    if (onlyA.size == 1 && onlyB.size == 1) {
      val litA = onlyA.head
      val litB = onlyB.head

      if (negate(litA) == litB) Some(a intersect b) else None
    } else None
  }

  private def negate(literal: String): String = if (literal.startsWith("!")) literal.drop(1) else "!" + literal

  private def removeNegation(literal: String): String = if (literal.startsWith("!")) literal.drop(1) else literal

  def getPrimeImplicants(dfTerms: Seq[Seq[String]]): Seq[Seq[String]] = {
    if (dfTerms.size <= 1) {
      // No simplification is possible
      dfTerms

    } else {
      // Simplify the expression with the Quine McCluskey algorithm.

      // Creates the map to convert the variable names to indices.
      val variableNames: List[String] = dfTerms.flatMap((terms: Seq[String]) => terms.map((variable: String) => removeNegation(variable))).toList.distinct
      val variableNameMap: Map[String, Int] = variableNames.zipWithIndex.toMap
      val numberOfVariableNames: Int = variableNames.size

      // Converts the DF termes to logic strings made of '0', '1' and '-' (0 and 1 at the same time)
      val allTrue: String = "-" * numberOfVariableNames
      val logicStrings: List[String] = dfTerms.map((term: Seq[String]) => {
        var logicString: String = allTrue
        for (variable <- term) {
          val variableName = removeNegation(variable)
          logicString = logicString.updated(variableNameMap(variableName), if (variable.startsWith("!")) '0' else '1')
        }
        logicString
      }).toList

      // Groups the logic string base on the number of contained '1' and wildcards ('-').
      val logicStringsGroupedByWildcards: mutable.Map[Int, mutable.Map[Int, mutable.Map[String, Set[Int]]]] = mutable.Map.from(logicStrings
        .zipWithIndex // List[(String, Int)]
        .map((logicString, refIndices) => (logicString, Set(refIndices))) // List[(String, Set[Int])]
        .groupBy((logicString, refIndices) => logicString.count(_ == '-')) // Map[Int, List[(String, Set[Int])]]
        .map((wildcards: Int, logicStringGroup: List[(String, Set[Int])]) => { // Map[Int, Map[Int, Map[String, Set[Int]]]]
          val logicStringGroupByNumberOfOnes: mutable.Map[Int, mutable.Map[String, Set[Int]]] = mutable.Map.from(logicStringGroup
            .groupBy((logicString: String, refIndices: Set[Int]) => logicString.count(_ == '1')) // Map[Int, List[(String, Set[Int]]
            .map((zeros: Int, expression: List[(String, Set[Int])]) => (zeros, mutable.Map.from(expression))))
          (wildcards, logicStringGroupByNumberOfOnes)
        }))

      // Calculates the prime implicants.
      var wildcards: Int = 0
      val nonPrimeImplicantsLogicStrings: mutable.Set[String] = mutable.Set.empty[String]
      val primeImplicantsLogicStrings: mutable.Map[String, Set[Int]] = mutable.Map.empty[String, Set[Int]]
      //while (logicStringsGroupedByWildcards.nonEmpty && logicStringsGroupedByWildcards.size > wildcards) {
      for (wildcardIndex: Int <- 0 until numberOfVariableNames) {
        wildcards = wildcardIndex
        val currentLogicStringsGroupedByOnes: mutable.Map[Int, mutable.Map[String, Set[Int]]] = logicStringsGroupedByWildcards.getOrElse(wildcards, mutable.Map.empty[Int, mutable.Map[String, Set[Int]]])
        wildcards += 1

        currentLogicStringsGroupedByOnes.size match {
          case 0 =>
          // No logic expressions defined => nothing to do
          case 1 =>
            // All logic strings are prime implicants.
            val keys: String = currentLogicStringsGroupedByOnes.mkString(", ")
            currentLogicStringsGroupedByOnes(currentLogicStringsGroupedByOnes.keys.head).foreach((logicString: String, refIndices: Set[Int]) => {
              Console.flush()
              if (primeImplicantsLogicStrings.contains(logicString)) {
                primeImplicantsLogicStrings(logicString) ++= refIndices
              } else {
                primeImplicantsLogicStrings.addOne((logicString, refIndices))
              }
            })
          case _ =>
            val nextLogicStringsGroupedByOnes: mutable.Map[Int, mutable.Map[String, Set[Int]]] = if (logicStringsGroupedByWildcards.contains(wildcards)) {
              logicStringsGroupedByWildcards(wildcards)
            } else {
              mutable.Map.empty[Int, mutable.Map[String, Set[Int]]]
            }
            val lowString: String = currentLogicStringsGroupedByOnes.mkString(", ")
            val upString: String = currentLogicStringsGroupedByOnes.mkString(", ")

            // The logic strings have to be compared pairwise inorder to simplify the logic expression.
            val allOneGroupIndices: Seq[Int] = currentLogicStringsGroupedByOnes.keys.toSeq.sorted
            for (lowerOneGroupIndex: Int <- allOneGroupIndices) {
              if (allOneGroupIndices.contains(lowerOneGroupIndex + 1)) {
                // If the comparison for two one logic string groups is necessary
                // Prepares everything for the simplification of the one logic string group.
                var lowerLogicStringGroup: mutable.Map[String, Set[Int]] = currentLogicStringsGroupedByOnes(lowerOneGroupIndex)
                val upperLogicStringGroup: mutable.Map[String, Set[Int]] = currentLogicStringsGroupedByOnes(lowerOneGroupIndex + 1)

                // Compare logic strings pairwise inorder to simplify the logic expression and determine the prime implicants.
                val newSimplifiesLogicStrings = upperLogicStringGroup.map((logicString: String, refIndices: Set[Int]) => {
                  // Checks all possible logic string simplifications for the current logic string
                  for (variableNameIndex <- 0 to numberOfVariableNames) {
                    if (logicString.charAt(variableNameIndex) == '0') {

                      // Checks if the twin logic string exist with a one at the position variableNameIndex and retrieves the ref indices.
                      val twinLogicString: String = logicString.patch(variableNameIndex, "1", 1)
                      val otherRefIndices: Set[Int] = upperLogicStringGroup
                        .filter((otherLogString: String, _: Set[Int]) => twinLogicString.equals(otherLogString))
                        .flatMap((_: String, otherRefIndices: Set[Int]) => otherRefIndices).toSet

                      if (otherRefIndices.nonEmpty) {
                        // If the twin logic string exist.
                        // Saves the new simplified logic string.
                        val newLogString: String = logicString.patch(variableNameIndex, "-", 1)
                        val newRefIndices: Set[Int] = refIndices ++ otherRefIndices
                        if (nextLogicStringsGroupedByOnes.contains(lowerOneGroupIndex)) {
                          val currentLogicStringsGroupedByOne: mutable.Map[String, Set[Int]] = nextLogicStringsGroupedByOnes(lowerOneGroupIndex)
                          val currentRefIndices: Set[Int] = currentLogicStringsGroupedByOne.getOrElse(newLogString, Set.empty[Int])
                          currentLogicStringsGroupedByOne(newLogString) = currentRefIndices ++ newRefIndices
                        } else {
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
                })

              } else {
                // If no further simplification comparisons for the current one logic string groups are required.
                // Adds the logic strings to the prime implicants if they were not used in a reduction.
                currentLogicStringsGroupedByOnes(lowerOneGroupIndex)
                  .filterNot((logicString: String, _: Set[Int]) => nonPrimeImplicantsLogicStrings.contains(logicString))
                  .foreach((logicExpression: (String, Set[Int])) => primeImplicantsLogicStrings.addOne(logicExpression))
              }
            }
            if (nextLogicStringsGroupedByOnes.nonEmpty) {
              logicStringsGroupedByWildcards.addOne((wildcards, nextLogicStringsGroupedByOnes))
            }
        }
      }

      // Finds the minimal prime implicant combination.
      val minimalPrimeImplicants: Seq[String] = primeImplicantsLogicStrings.toSeq
        .filterNot((logicString: String, refIndices: Set[Int]) => {
          primeImplicantsLogicStrings.exists((otherLogicString: String, otherRefIndices: Set[Int]) =>
            (logicString != otherLogicString) && refIndices.subsetOf(otherRefIndices))
        }).map((logicString: String, refIndices: Set[Int]) => logicString)

      // Reconstruct logical expression from the minimal prime implicant logic strings.
      val dmf: Set[Set[String]] = minimalPrimeImplicants.map((logicString: String) => {
        logicString.toList.zipWithIndex.filterNot((alg: Char, variableNameIndex: Int) => alg == '-')
          .map((alg: Char, variableNameIndex: Int) => {
            val variableName: String = variableNames(variableNameIndex)
            if ((alg == '0') == variableName.startsWith("!")) variableName else negate(variableName)
          }).toSet
      }).toSet

      dmf.map((part: Set[String]) => part.toSeq).toSeq
    }
  }
}
