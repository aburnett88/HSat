# HSat

HSat is a library used for solving and analysing Boolean Satisfiability Problems - or atleast it is intended to be so.

## Latest Release Features

* Nothing. This is not suitable for anything yet, other than learning

# Complete Change Log

## Pre-version 1.0

### 0.0.0.2

* Library
  * Added HSat.Data.BSP.CNF.Builder
    * Allows CNF types to be 'built up' and errors caught
  * Added HSat.Data.BSP.CNF.Parser
    * Allows ".cnf" files to be parsed


### 0.0.0.1 Date: 29/11/2014

* Documentation
  * Updated GitHub README text
  * Add a LICENSE file to the project
  * 100% coverage of documentation, as ran under haddock
* Organisation
  * Created src and testing directories
* Cabal & Haskell
  * Create minimal cabal file
  * Set up cabal sandbox
* Library
  * Added HSat.Data.BSP
    * General Boolean Satisfiable Problem data type. Is intended to be extendable with a general Formula type, DNF
  * Added HSat.Data.BSP.CNF
    * Used to describe CNF instances. For more specific information, consult the documentation
  * Added HSat.Data.BSP.Common
    * Used for importing Clause, Clauses, Literal, Sign, Variable without having to import each name separately.     * Contains general 
  * Added HSat.Data.BSP.Common.Clause
    * Used to hold a general list of Literal's. For more specific information, consult the documentation
  * Added HSat.Data.BSP.Common.Clauses
    * Used to hold a general list of Clause's. For more specific information, consult the documentation.
  * Added HSat.Data.BSP.Common.Literal
    * Used to hold a Variable and a Sign. For more specific information, consult the documentation.
  * Added HSat.Data.BSP.Common.Sign
    * Used as a specific data type denoting whether a Literal is a positive or negative occurence of a Variable. For more specific information, consult the documentation.
  * Added HSat.Data.BSP.Common.Variable
    * Used as a specific data type denoting a Variable's integer representation. For more specific information, consult the documentation.


### 0.0.0.0

* Organisation
  * Created GitHub repository

