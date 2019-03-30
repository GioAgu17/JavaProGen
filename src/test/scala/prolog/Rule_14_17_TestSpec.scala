package prolog
import Check.ops._

import org.scalatest.FlatSpec

class Rule_14_17_TestSpec extends FlatSpec {
  // cannot have a return type different than void
  "JLS Rule 14.17" should "fail for this code fragment" in {
    val code = "public class A {" +
      "public void m1(int a, int b){" +
      "return a+b;" +
      "}" +
      "}"
    val r: Boolean = code.checkWithKB("14.17")
    assert(!r)
  }
  "JLS Rule 14.17" should "pass for this code fragment" in {
    val code = "public class A {" +
      "public int m1(int a, int b){ " +
      "return a + b;" +
      "}" +
      "}"
    val r: Boolean = code.checkWithKB("14.17")
    assert(r)
  }
}




//  "JLS Rule 14.17" should "fail also for this code fragment" in{
//    val code = "public class A {" +
//                  "public int m3(int a, int b) {" +
//                      ";" +
//                  "}" +
//                "}"
//    val r: Boolean = code.checkWithKB("14.17")
//    assert(!r)
//  }
//
//}
