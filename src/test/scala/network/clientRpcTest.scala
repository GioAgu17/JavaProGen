package network

import io.grpc.ConnectivityState
import network.server.ServerRPC
import org.scalatest.FunSuite
import progen.prolog.ClientRpc

import scala.io.Source

class clientRpcTest extends FunSuite {
  test("testing connection"){
    // reading Prolog file
    val bufferedSrc = Source.fromFile("src/main/resources/prolog.txt")
    val rules = bufferedSrc.getLines().toList
    bufferedSrc.close()
    val clientRpc = new ClientRpc
    ServerRPC.start()
    val test = clientRpc.setRules(rules)
    assert(test)
    clientRpc.addNodeToProlog("compilationunit",0,0,0)


    assert(clientRpc.channel.getState(true) == ConnectivityState.READY)
  }
}
