package prolog
import Check.ops._

import org.scalatest.FlatSpec

class Rule_8_4_1_TestSpec extends FlatSpec{
  "JLS Rule 8.4.1" should "fail for this code fragment" in{
    val r: Boolean = "public class A{ public void m(int s, char s){ return; } }".checkWithKB("8.4.1")
    assert(! r)
  }
  "JLS Rule 8.4.1" should "pass for this code fragment" in{
    val r: Boolean = "public class A{ public void m(int m, char s){ return; } }".checkWithKB("8.4.1")
    assert( r)
  }

}
