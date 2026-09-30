# SuperJoern Install Guide
## Preliminaries:
- Git
- IntelliJ (tested with 2025.1.4.1) with the Scala plugin installed

- Install SBT and JDK (tested with 1.11.3 (Ubuntu Java 21.0.9)):
```
    sudo apt-get update
    
    sudo apt-get install apt-transport-https curl gnupg -yqq
    
    echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
    
    echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
    
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo -H gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scalasbt-release.gpg --import
    
    sudo chmod 644 /etc/apt/trusted.gpg.d/scalasbt-release.gpg
    
    sudo apt-get update
    
    sudo apt-get install sbt
    
    sudo apt install openjdk-21-jdk
```


## Install:
1. Clone our Joern Project:
```
    git clone https://github.com/KIT-TVA/joern.git
```


2. Go into the joern directory
```
    cd joern/
```


3. Clone our SuperC project into Joern
```
    git clone git@gitlab.kit.edu:kit/tva/baechle/student-work/master/2026-ma-dormann-superc.git
```


4. Rename the superC project:
```
    mv 2026-ma-dormann-superc/ superc
```


5. Install libaries required by superC and copy them into the bin directory
```
    sudo apt-get install -y libz3-java=4.8.12-3.1build1 libjson-java sat4j bison
    
    cd superc
    
    # Please check if all 3 libraries are named the same. If not, update the names in the following command.
    cp /usr/share/java/org.sat4j.core.jar /usr/share/java/com.microsoft.z3-4.8.12.0.jar /usr/share/java/json-lib.jar bin
```



5. Clone our CodePropertyGraph project
```
    cd ../..
    
    git clone https://github.com/KIT-TVA/codepropertygraph.git

  

    cd codepropertygraph/

    sudo apt install git-lfs
    
    git lfs pull
```



6. Compile and publish the CodePropertyGraph project locally
```
    sbt clean test publishM2
```


7. Go back into the Joern directory
```
    cd ../joern
```


8. Open the SBT console
```
    sbt
```


9. Compile the project and keep the SBT console open!
```
    compile
```


10. With the SBT console open, open the Joern Project in IntelliJ. When prompted to either use SBT or BSP import, select BSP.



11. Once IntelliJ finished the import, you can close the sbt console and should be able to compile the project in IntelliJ.


---

## Usage & Development

### Development
- The SuperC frontend is implemented as part of the C frontend.
  The implementation of the frontend is located at [```joern/joern-cli/frontends/c2cpg/src/main/scala/io.joern.c2cpg/astcreation/```](joern/joern-cli/frontends/c2cpg/src/main/scala/io.joern.c2cpg/astcreation/). The enty point for the VA-AST is the [```VAstCreatorNew.scala```](joern/joern-cli/frontends/c2cpg/src/main/scala/io.joern.c2cpg/astcreation/VAstCreatorNew.scala).
  All defined ```VAstPatternConverters``` and ```VAstFeatureHandler``` are located in [```joern/joern-cli/frontends/c2cpg/src/main/scala/io.joern.c2cpg/astcreation/converter```](joern/joern-cli/frontends/c2cpg/src/main/scala/io.joern.c2cpg/astcreation/converter) and are registerres/initizialted in [```joern/joern-cli/frontends/c2cpg/src/main/scala/io.joern.c2cpg/astcreation/VAstConverterForC.scala```](joern/joern-cli/frontends/c2cpg/src/main/scala/io.joern.c2cpg/astcreation/VAstConverterForC.scala)
  A ```VAstPatternConverters``` handles the conversion of one C-Feature as listed in the table below. A ```VAstFeatureHandler```provides functionality that is required by multiple ```VAstPatternConverter```s.

- The corresponding tests are located at [```joern/joern-cli/frontends/c2cpg/src/main/test/scala/io.joern.c2cpg/variability```](joern/joern-cli/frontends/c2cpg/src/main/test/scala/io.joern.c2cpg/variability).

- Furthermore, changes to the derivation process of the CFG have been made in ```src/main/scala/io/joern/x2cpg/passes/controlflow/cfgcreation/CfgCreator.scala``` and the PDG annotation pass is located at ```src/main/scala/io/joern/c2cpg/passes/variability/PdgPresenceConditionAnnotationPass.scala```.

- The SuperC VA-AST plotting implementation is lacated in [```joern/joern-cli/frontends/c2cpg/src/main/test/scala/io.joern.c2cpg/variability/util/TestUtil.scala```](joern/joern-cli/frontends/c2cpg/src/main/test/scala/io.joern.c2cpg/variability/util/TestUtil.scala). If only a SuperC VA-AST should be ploted ```TestUtil.superCGraphToDotGraph(...)``` an be used, but it is recomende to use ```TestUtil.generateVASTDot(...)```, ```TestUtil.generateVCFGDot(...)```, ```TestUtil.generateVPDGDot(...)```, ```TestUtil.generateVCPGDot(...)``` to abstrct from the aditional complexity event if the variabiable JOERN GRAPH is also returned.

- The JOERN AST, CFG, PDG, CPG, VA-AST, VA-CFG, VA-PDG and VA-CPG can be plotted with ```DotAstGenerator.dotAst(...)``` and ```DotCpg14Generator.toDotCpg14(...)```. The JOERN dot-graph generator extensions are located in [```joern/joern-cli/frontends/c2cpg/src/test/scala/io/joern/c2cpg/variability/vast/DotSerializer.scala```](joern/joern-cli/frontends/c2cpg/src/test/scala/io/joern/c2cpg/variability/vast/DotSerializer.scala).

### Test-Case Structure
```scala
package io.joern.c2cpg.variability.vast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.joern.c2cpg.variability.util.TestUtil.generateVASTDot
import io.joern.x2cpg.testfixtures.TestCpg
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.dotgenerator.DotAstGenerator

class testComplexFunctionReturnTypes extends C2CpgSuite(withOssDataflow = true) {

  /////////////////////////
  //    C-Sample Code    //
  /////////////////////////
  // Defines the C sample.
  val cFileName: String = "Test.c"
  val cCode: String =
    """
      |void foo() {
      |  int x = source(); // Attacker−controlled.
      |  if(x < MAX){ // Does not enforce x >= 0.
      |    int y = 0;
      |#ifdef CONFIG_PROCESS_INPUT
      |    y = 2 * x;
      |  #ifdef CONFIG_SEND_DATA
      |    sink(y); // Security−sensitive operation.
      |  #endif
      |#endif
      |    // ...
      |  }
      |}
      |
      |""".stripMargin


  /////////////////////////////////////////////////////////////////////////
  //    JOERN C-Frontend Implementation (without Variability Support)    //
  /////////////////////////////////////////////////////////////////////////
  // Creates the CPG (together with AST, CFG and PDG) using the default C-Frontend of JOERN.
  val cCpg: TestCpg = code(cCode, cFileName)

  // Splits the returned JOERN CPG into several Sub-CPGs that start with a method declaration node (node kind 25),
  // including the "<global>" method declaration node, that contains all variable and method declarations.
  val cTraversal: Iterator[Method] = cCpg.graph._nodes(25).asInstanceOf[Iterator[nodes.Method]]

  // Returns for each of the method declaration sub-CPGs the AST as a dot-graph with node coloring (withColoring = true)
  // and all node-specific parameters, except for graph-related parameters (extendedView = true).
  val cAstDotString: Iterator[String] = DotAstGenerator.dotAst(cTraversal, extendedView = true, withColoring = true)
  println("Standard JOERN C AST:")
  println(cAstDotString.mkString)

  // Returns for each of the method declaration sub-CPGs the CPG as a dot-graph with node coloring (withColoring = true),
  // edge coloring and all node-specific parameters, except for graph-related parameters (extendedView = true).
  val cCpgDotString: Iterator[String] = DotCpg14Generator.toDotCpg14(cTraversal, extendedView = true, withColoring = true)
  println("Standard JOERN C CPG:")
  println(cCpgDotString.mkString)



  ///////////////////////////////////////////////////////////////////
  //    Variable C-Frontend Implementation (out Implementation)    //
  ///////////////////////////////////////////////////////////////////
  // Creates the JOERN VA-CPG (together with VA-AST, VA-CFG and VA-PDG) using the new variable C-Frontend of JOERN and
  // returns the SuperC VA-AST and JOERN VA-ASTs (extendedView = true, withColoring = true) as dot-graphs.
  val (superCAstDotString: String, superCJoernAstDotString: String) = generateVASTDot(cCode, cFileName)

  println("\nSuperC VA-AST (original data structure):")
  println(superCAstDotString)
  println("\nSuperC-JOERN VA-AST (translated to JOERN VA-AST data structure):")
  println(superCJoernAstDotString)


  // Creates the JOERN VA-CPG (together with VA-AST, VA-CFG and VA-PDG) using the new variable C-Frontend of JOERN and
  // returns the SuperC VA-AST and JOERN VA-CPGs (extendedView = true, withColoring = true and edge coloring) as
  // dot-graphs.
  val (superCCpgDotString: String, superCJoernCpgotString: String) = generateVASTDot(cCode, cFileName)

  println("\nSuperC VA-AST (original data structure):")
  println(superCCpgDotString)
  println("\nSuperC-JOERN VA-CPG (translated to JOERN VA-CPG data structure):")
  println(superCJoernCpgotString)
}

```




---

## Current State

### Supported C-Feature
| Task | Description | Status |
|:---:|:---|:---:|
| 1 | unary operations handling | <span style="color:green">done</span> |
| 2 | binary operations handling | <span style="color:green">done</span> |
| 3 & 4 | multi variable declaration | <span style="color:green">done</span> |
| 5 | ```typedef``` and ```struct``` declaration | <span style="color:red">todo</span> |
| 6 | assembly code and register access handling [optional] | <span style="color:red">todo</span> |
| 7 | ```enum``` declaration | <span style="color:green">done</span> |
| 8 | ```goto``` and ```goto label``` handling | <span style="color:green">done</span> |
| 9 | multi file support [optional] | <span style="color:red">todo</span> |
| 10 & 15 | multi function declaration | <span style="color:orange">ongoing</span> |
| 11 | code generation/reconstruction | <span style="color:green">done</span> |
| 12 | ```if``` statements | <span style="color:green">done</span> |
| 13 | ```switch```-case statements | <span style="color:green">done</span> |
| 14 | code block handling | <span style="color:green">done</span> |
| 16 | function call handling | <span style="color:green">done</span> |
| 17 | static assertions [optional] | <span style="color:red">todo</span> |
| 18 | dot-graph generation for JOERN and SuperC | <span style="color:green">done</span> |
| 19 | ```while``` and ```do-while``` loop handling | <span style="color:green">done</span> |
| 20 | ```for``` loop handling | <span style="color:green">done</span> |
| 21 | ```breake```, ```continue``` instruction handling | <span style="color:green">done</span> |
| 22 | simple parameterized macro handling | <span style="color:red">todo</span> |
| 23 | conditional macros handling | <span style="color:green">done</span> |

More detailed information on the software architecture, the current status, and the background can be found in the PDF [Family-Based_Vulnerability_Discovery_for_HCSS.pdf](doc/Family-Based_Vulnerability_Discovery_for_HCSS.pdf) and the presentation [Family-Based_Vulnerability_Discovery_for_HCSS_(slides).pdf](doc/Family-Based_Vulnerability_Discovery_for_HCSS_(slides).pdf).

### General notes on the implementation:
1. Each C feature has its own ```VAstPatternConverter```, which implements ```io.joern.c2cpg.astcreation.converter.VAstPatternConverter``` and must be registered in ```VASTConverterForC```.
2. Functionality required by multiple C features is outsourced to so-called ```VAstFeatureHandler```s, which also has to be registered in ```VASTConverterForC```.
3. The location information methods in ```VASTCreatorNew``` should not be used, because it is simpler to extract the code positions directly from the SuperC AST (from the Language and Text nodes).
4. JOERN Choice nodes can now have more than 2 sub-ASTs and never have a code position specified.
5. JOERN return nodes have a code position specification only if they have a return argument; otherwise, they have no code position specification.
6. The JOERN dot-graph plotting functionality now offers options for coloring important nodes, outputting important node parameters, or edge-type coloring.
6. The SuperC AST can now also be output as a dot graph.
7. Red colored JOERN nodes in the VAST or other JOERN graphs indicate an incomplete translation caused by missing C feature support.
8. All tests are located in the package: ```io.joern.c2cpg.variability.vast```

### Notes on remaining tasks and adjustments to the other graph creators (CFG, ...):
1. It is recommended to implement ```typedef``` and ```struct``` declarations as ```VAstFeatureHandler``` and, similar to the ```VAstPatternConverterForConditionalMacro```, to define an additional ```VASTPatternConverter``` that internally uses the ```VAstFeatureHandler```, because ```typedef``` and ```struct``` declarations can be combined with variable declarations, meaning that a ```typedef``` or ```struct``` declaration can be a sub-AST of a variable declaration or a multi-variable declaration.
2. It is essential to ensure that control flows and data dependencies are handled correctly for complex features such as method declarations or switch cases, especially since a major adjustment has been made in the modeling of conditional return types for method declarations and ```Choice``` nodes can now have more than two children.
3.  It is recommended to adjust the function parameter declaration again and use a structure similar to that of variable accesses (JOERN ```Identifier``` nodes) if the parameters are conditional.
4. The conditional handling of the conditional return-type pointer degree and the conditional decision of whether an array is returned is still missing and needs to be added.
5. The handling of array initializations needs to be added.





---



Standard Joern's original readme: Joern - The Bug Hunter's Workbench
===

[![release](https://github.com/joernio/joern/actions/workflows/release.yml/badge.svg)](https://github.com/joernio/joern/actions/workflows/release.yml)
[![Joern SBT](https://index.scala-lang.org/joernio/joern/latest.svg)](https://index.scala-lang.org/joernio/joern)
[![Github All Releases](https://img.shields.io/github/downloads/joernio/joern/total.svg)](https://github.com/joernio/joern/releases/)
[![Gitter](https://img.shields.io/badge/-Discord-lime?style=for-the-badge&logo=discord&logoColor=white&color=black)](https://discord.com/invite/vv4MH284Hc)

Joern is a platform for analyzing source code, bytecode, and binary
executables. It generates code property graphs (CPGs), a graph
representation of code for cross-language code analysis. Code property
graphs are stored in a custom graph database. This allows code to be
mined using search queries formulated in a Scala-based domain-specific
query language. Joern is developed with the goal of providing a useful
tool for vulnerability discovery and research in static program
analysis.

Website: https://joern.io

Documentation: https://docs.joern.io/

Specification: https://cpg.joern.io

## News / Changelog

- Joern v4.0.0 [migrates from overflowdb to flatgraph](changelog/4.0.0-flatgraph.md)
- Joern v2.0.0 [upgrades from Scala2 to Scala3](changelog/2.0.0-scala3.md)
- Joern v1.2.0 removes the `overflowdb.traversal.Traversal` class. This change is not completely backwards compatible. See [here](changelog/traversal_removal.md) for a detailed writeup.

## Requirements

- JDK 21 (other versions _might_ work, but have not been properly tested)
- _optional_: gcc and g++ (for auto-discovery of C/C++ system header files if included/used in your C/C++ code)

## Quick Installation

```
wget https://github.com/joernio/joern/releases/latest/download/joern-install.sh
chmod +x ./joern-install.sh
sudo ./joern-install.sh
joern

     ██╗ ██████╗ ███████╗██████╗ ███╗   ██╗
     ██║██╔═══██╗██╔════╝██╔══██╗████╗  ██║
     ██║██║   ██║█████╗  ██████╔╝██╔██╗ ██║
██   ██║██║   ██║██╔══╝  ██╔══██╗██║╚██╗██║
╚█████╔╝╚██████╔╝███████╗██║  ██║██║ ╚████║
 ╚════╝  ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝
Version: 2.0.1
Type `help` to begin

joern>
```

If the installation script fails for any reason, try
```
./joern-install --interactive
```

## Development Requirements
- [java](https://jdk.java.net/)
- [sbt](https://www.scala-sbt.org)

## Run unit and integration tests locally
Unit tests:
```bash
sbt test
```

Integration tests:
```bash
sbt joerncli/stage querydb/createDistribution
python -m pip install requests pexpect # wexpect on Windows
python -u ./testDistro.py
```

## Docker based execution

```
docker run --rm -it -v /tmp:/tmp -v $(pwd):/app:rw -w /app -t ghcr.io/joernio/joern joern
```

To run joern in server mode:

```
docker run --rm -it -v /tmp:/tmp -v $(pwd):/app:rw -w /app -t ghcr.io/joernio/joern joern --server
```

Almalinux 9 requires the CPU to support SSE4.2. For kvm64 VM use the Almalinux 8 version instead.
```
docker run --rm -it -v /tmp:/tmp -v $(pwd):/app:rw -w /app -t ghcr.io/joernio/joern-alma8 joern
```

## Releases
A new release is [created automatically](.github/workflows/release.yml) once per day. Contributers can also manually run the [release workflow](https://github.com/joernio/joern/actions/workflows/release.yml) if they need the release sooner.

## Developers

### Contribution Guidelines

Thank you for taking time to contribute to Joern! Here are a few guidelines to ensure your pull request will get merged as soon as possible:

* Try to make use of the templates as far as possible, however they may not suit all needs. The minimum we would like to see is:
    - A title that briefly describes the change and purpose of the PR, preferably with the affected module in square brackets, e.g. `[javasrc2cpg] Addition Operator Fix`.
    - A short description of the changes in the body of the PR. This could be in bullet points or paragraphs.
    - A link or reference to the related issue, if any exists.
* Do not:
    - Immediately CC/@/email spam other contributors, the team will review the PR and assign the most appropriate contributor to review the PR. Joern is maintained by industry partners and researchers alike, for the most part with their own goals and priorities, and additional help is largely volunteer work. If your PR is going stale, then reach out to us in follow-up comments with @'s asking for an explanation of priority or planning of when it may be addressed (if ever, depending on quality).
    - Leave the description body empty, this makes reviewing the purpose of the PR difficult.
* Remember to:
    - Remember to format your code, i.e. run `sbt scalafmt Test/scalafmt`
    - Add a unit test to verify your change.

### IDE setup

#### Intellij IDEA
* [Download Intellij Community](https://www.jetbrains.com/idea/download)
* Install and run it
* Install the [Scala Plugin](https://plugins.jetbrains.com/plugin/1347-scala) - just search and install from within Intellij.
* Important: open `sbt` in your local joern repository, run `compile` and keep it open - this will allow us to use the BSP build in the next step
* Back to Intellij: open project: select your local joern clone: select to open as `BSP project` (i.e. _not_ `sbt project`!)
* Await the import and indexing to complete, then you can start, e.g. `Build -> build project` or run a test

#### VSCode
- Install VSCode and Docker
- Install the plugin `ms-vscode-remote.remote-containers`
- Open Joern project folder in VSCode
  - [Option 1](https://docs.microsoft.com/en-us/azure-sphere/app-development/container-build-vscode#build-and-debug-the-project): Visual Studio Code detects the new files and opens a message box saying: `Folder contains a Dev Container configuration file. Reopen to folder to develop in a container.`. Select the `Reopen in Container` button to reopen the folder in the container created by the `.devcontainer/Dockerfile` file.
  - Option 2: press `Ctrl + Shift + P` then select `Dev Containers: Reopen in Container`
- Press `Ctrl + Shift + P` then select `Metals: Import build`
- After `Metals: Import build` succeeds, you are ready to start writing code for Joern

## QueryDB (queries plugin)
Quick way to develop and test QueryDB:
```
sbt stage
./querydb-install.sh
./joern-scan --list-query-names
```
The last command prints all available queries - add your own in querydb, run the above commands again to see that your query got deployed.
More details in the [separate querydb readme](querydb/README.md)
