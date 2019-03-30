package prolog
import Check.ops._

import org.scalatest.FlatSpec

class Rule_8_8_7_TestSpec extends FlatSpec{
  "JLS Rule 8.8.7" should "fail for this code fragment" in{
    val code = "public class A{" +
                  "A(int m,int s){" +
                      "this(1,2);" +
                  "}" +
              "}"

    val r: Boolean = code.checkWithKB("8.8.7")
    assert(! r)
  }
  "JLS Rule 8.8.7" should "pass for this code fragment" in{
    val code = "public class B{" +
                    "B(int m){" +
                        "m = 3;" +
                    "}" +
                    "B(int a, int c){" +
                          "this(a);" +
                          "c = 2;" +
                    "}" +
                "}"
    val r: Boolean = code.checkWithKB("8.8.7")
    assert(r)
  }
}
