package prolog
import Check.ops._

import org.scalatest.FlatSpec

class Rule_8_8_2_TestSpec  extends FlatSpec{
  "JLS Rule 8.8.2" should "fail for this code fragment" in{
      val code = "public class A {" +
                      "A(int m, char s){" +
                            "m = 3;" +
                            "s = 'a';" +
                      "}" +
                      "A(int a, char b){" +
                          ";" +
                      "}" +
                  "}"
    val r: Boolean = code.checkWithKB("8.8.2")
    assert(!r)
  }
  "JLS Rule 8.8.2" should "pass for this code fragment" in{
    val code = "public class B{ " +
                    "B(int m, int s){" +
                          "m = 3;" +
                          "s = 2;" +
                    "}" +
                    "B(int s, char a){" +
                        "s = 1;" +
                        "a = 'a';" +
                    "}" +
                "}"
    val r: Boolean = code.checkWithKB("8.8.2")
    assert(r)
  }

}
