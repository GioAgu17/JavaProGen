package prolog
import Check.ops._
import org.scalatest.FlatSpec

class Rule_8_4_TestSpec extends FlatSpec {


  // Compile Time Error for the body of a class to declare as members two methods with override equivalent signatures


  "JLS Rule 8.4" should "fail for this code fragment" in{
    val code = "public class A{ " +
                  "public int m(char s, int b){" +
                      " return 3; " +
                "} public int m(char e, int f){" +
                      " return 2; " +
                "}" +
              "}"
    val r: Boolean = code.checkWithKB("8.4")
    assert( !r)
  }
// if the two override-equivalent methods are in different classes, no error should be triggered
  "JLS Rule 8.4" should "pass for this code fragment" in{
    val code = "public class A{ " +
                  "public int m(char s, int b){" +
                      " return 3; " +
                  "}" +
                "}" +
                "public class B{" +
                    "public int m(char e, int f){" +
                      " return 2; " +
                    "}" +
                "}"
    val r: Boolean = code.checkWithKB("8.4")
    assert(r)
  }

}
