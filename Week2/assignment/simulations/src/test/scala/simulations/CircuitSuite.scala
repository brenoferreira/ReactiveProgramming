package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(true)
    in2.setSignal(false)
    run

    assert(out.getSignal === true, "and 1")

    in1.setSignal(false)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(true)
    in2.setSignal(false)
    run

    assert(out.getSignal === true, "and 1")

    in1.setSignal(false)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("demux test") {
    val in = new Wire
    val c1 = new Wire
    val c = List(c1)

    val out1, out2 = new Wire
    val out = List(out1, out2)

    demux(in, c, out)

    in.setSignal(true)

    run

    List(out1, out2).zip(
    List(false, true)) foreach (wb => {
      //println(wb._1.getSignal)
      assert(wb._1.getSignal === wb._2, "demux 1")
    })
  }

  test("demux2 test") {
    val in, c1, c2, out11, out12, out21, out22 = new Wire
    val outs = List(out22, out21, out12, out11)
    demux(in, List(c2, c1), outs)

    c1.setSignal(false)
    c2.setSignal(false)
    in.setSignal(true)

    run

    outs.zip(
    List(false, false, false, true)) foreach (wb => {
      //println(wb._1.getSignal)
      assert(wb._1.getSignal === wb._2, "demux 2")
    })
  }

  test("demux3 test") {
    val in, c1, c2, out11, out12, out21, out22 = new Wire
    val outs = List(out22, out21, out12, out11)
    demux(in, List(c2, c1), outs)

    c1.setSignal(false)
    c2.setSignal(true)
    in.setSignal(true)

    run

    outs.zip(
      List(false, true, false, false)) foreach (wb => {
      //println(wb._1.getSignal)
      assert(wb._1.getSignal === wb._2, "demux 3")
    })
  }
}
