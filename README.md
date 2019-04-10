# ProGen

Random Java programs generator using Prolog-based grammar validation and functional programming.

## Getting Started

These instructions will get you a copy of the project up and running on your
local machine for development and testing purposes.

### Prerequisites

You need to install the latest SBT version along with Scala on your computer 
or the SBT plugin on the IDE you are using. SBT will handle all the project 
dependencies building the project from scratch. IntelliJ IDE is recommended as
you will see all the code and you can also modify some parameters to see what
changes are applied by the application.
However if you don't want to use it you can also run the project
on your os shell. Next instructions will cover both environment settings.

### Installing with IntelliJ

After having cloned the repo on your local machine, load the /scala_project 
folder into an Intellij project. 


### Installing from SBT Shell 
If you are using a shell to run the sbt project go in the folder /scala-project 
and type
```
sbt clean compile test
```

Then run the application with
```
sbt run
```
This will let the application run and you will see the output of the application.

## Logging 
If you want to see the log of the application, just go to /src/main/resources/log/myapp.log. 
## Running the tests

###With IntelliJ
In the subdirectory `test`, there are all the unit tests. The package organization is the same
as the one in the `main` subdirectory. If you want to run all test, right click on the `scala` 
subdirectory inside `test` and then click Run -> Run all scala tests
###With SBT shell
To run all tests execute the sbt task `sbt test`. 

###Testing styles

The testing framework employed is ScalaTest version 3.0.1. Two different testing styles
have been adopted: FunSuite and FlatSpec.

As regards the unit tests of classes, FunSuite has been employed. For every function
of a Scala class/object, a test has been implemented. In the example below, we can see
how the test for the function 'genMethodParameters' is done in the `InterfaceGenerator` class:

```
class InterfaceGeneratorTest extends FunSuite{
    test("genParameters test"){
        val paramPrefix = configurationRetriever.getParamPrefix
        val params = interfaceGenerator.genParameters(possibleTypes)
        val types = params.map(_._1)
        types.foreach(t => assert(possibleTypes.contains(t)))
        val names = params.map(_._2)
        names.foreach(s => assert(s.contains(paramPrefix)))
      }
   // other tests
}   
```

For testing Prolog rules, FlatSpec has been used for the freedom of expressing the test
case with natural language. In the `mocapro/src/test/scala/prolog` subdirectory there are
the test cases for the JLS rules in Prolog. The way of testing the Prolog Knowledge Base
is straightforward: for every rule, two Java code fragments are given, one for a successful
response of the KB and the other for a not successful response of the KB. Using the ASTParser
Eclipse library, the code is parsed into an Abstract Syntax Tree and the Prolog query is 
built from the information given by the tree. The example below shows the properties of 
the testing approach just explained.
```$xslt
    class Rule_8_1_4_TestSpec extends FlatSpec{
      "JLS Rule 8.1.4.1" should "fail for this code fragment" in{
        val r: Boolean = "public class A extends A{ }".checkWithKB("8.1.4.1")
        assert( !r)
      }
      "JLS Rule 8.1.4.1" should "pass for this code fragment" in{
        val r: Boolean = "public class A extends B{ }".checkWithKB("8.1.4.1")
        assert(r)
      }
    }  
```
The function checkWithKB is an impliciitly declared thanks to this type class:
```$xslt
    trait Check[A] {
      def checkWithKB(a: A, rule: String): Boolean
    }
    
    object Check {
    
      def apply[A](implicit sh: Check[A]): Check[A] = sh
    
      object ops {
        def checkWithKB[A: Check](a: A)(rule: String) = Check[A].checkWithKB(a,rule: String)
    
        implicit class CheckOps[A: Check](a: A) {
          def checkWithKB(rule: String) = Check[A].checkWithKB(a,rule)
        }
      }
    
    
    
      implicit val stringCanCheck: Check[String] =
        (str,rule) => {
          val checker = CheckerFactory.getChecker(rule)
          checker.parseAndCheckKB(str)
        }
    
    }
```

The Checker Factory creates a Checker instance related to the rule that has to be checked.
In the case of the rule in the example, a specific class will be instantiated on which
the method `parseAndCheckKB` will be called for getting a response from the Prolog KB.
```
    class ExtendChecker1 extends Checker with ParserAST {
      /**
        * Instantiates an AST parser from the source code given in input,
        * traverses it with the Visitor pattern and ask the Prolog system if
        * the extend rule is satisfied
        * @param source the source code
        * @return true if the Prolog rule is satisfied by the source code, false otherwise
        */
        override def parseAndCheckKB(source: String): Boolean ={
    
          var response = false
          val cu = createParser(source)
          cu.accept(new ASTVisitor() {
            override def visit(node: TypeDeclaration): Boolean = {
              val typeName = node.getName
              if (!node.isInterface) {
                val superType = node.getSuperclassType
                if (superType != null) {
                  val superName = superType.toString
                  val query = "canExtend(" + typeName + "," + superName + ")."
                  response = engine.solve(query).isSuccess
                }
              }
              false
            }
          })
          response
        }
    }
```




## Built With


* [SBT](https://www.scala-sbt.org/) - Dependency Management
* [IntelliJ](https://www.jetbrains.com/idea/) - IntelliJ IDEA 2018.2.4 (Ultimate Edition)
                                                       JRE: 1.8.0_152-release-1248-b8 amd64
                                                       JVM: OpenJDK 64-Bit Server VM by JetBrains s.r.o
                                                       Windows 10 10.0



## Authors

* **Giovanni Agugini Bassi** - Master student at University of Illinois at Chicago -*Initial work* 



## Acknowledgments

* Great explanation of Scala syntax: https://alvinalexander.com/
* Design of type classes: https://blog.scalac.io/2017/04/19/typeclasses-in-scala.html
* Inspiration (TODO)


