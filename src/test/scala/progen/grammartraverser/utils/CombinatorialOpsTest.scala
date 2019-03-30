package progen.grammartraverser.utils

import org.scalatest.FunSuite
class CombinatorialOpsTest extends FunSuite {
  test("combination function") {
    val listTest = List(1, 2, 3, 4, 5)

    val actualList = List(List(1, 2), List(1, 3), List(1, 4), List(1, 5), List(2, 3), List(2, 4), List(2, 5), List(3, 4), List(3, 5), List(4, 5))
    val expectedList = listTest.combinations(2).toList
    assert(expectedList == actualList)
  }


}
