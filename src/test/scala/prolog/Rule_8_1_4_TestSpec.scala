package prolog
import Check.ops._
import org.scalatest.FlatSpec

class Rule_8_1_4_TestSpec extends FlatSpec{
  "JLS Rule 8.1.4.1" should "fail for this code fragment" in{
    val r: Boolean = "public class A extends A{ }".checkWithKB("8.1.4.1")
    assert( !r)
  }
  "JLS Rule 8.1.4.1" should "pass for this code fragment" in{
    val r: Boolean = "public class A extends B{ }".checkWithKB("8.1.4.1")
    assert(r)
  }
  "JLS Rule 8.1.4.2" should "fail for this code fragment" in{
    val r: Boolean = "public class A extends B{ } public class B extends A{ }".checkWithKB("8.1.4.2")
    assert( !r)
  }
  "JLS Rule 8.1.4.2" should "pass for this code fragment" in{
    val r: Boolean = "public class A extends B{ } public class B extends C{ }".checkWithKB("8.1.4.2")
    assert( r)
  }

}
