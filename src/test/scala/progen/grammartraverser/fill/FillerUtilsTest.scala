package progen.grammartraverser.fill

import progen.peg.entities.MethodSignature
import org.scalatest.FunSuite
class FillerUtilsTest extends FunSuite{
    test("findTrees function"){
      val fillerTest = new FillerTest
      val classTreesAndSymTabs = fillerTest.fillClasses
      val classTrees = classTreesAndSymTabs.map(_._1)
      val symTabs = classTreesAndSymTabs.map(_._2)
      val methDeclTreesForEveryClass = classTrees.map(classTreeTest => FillerUtils.findTrees(classTreeTest,"<methoddeclaration>"))
      val methodSignForEveryClass: List[List[MethodSignature]] = symTabs.map(s => s.symTabEntry.methods match{
        case Some(ms) => ms
        case None => List()
      })
      (methDeclTreesForEveryClass zip methodSignForEveryClass).foreach(s => assert(s._1.size == s._2.size))
    }
}
