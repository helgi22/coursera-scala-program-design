package ws

abstract class Gates extends Simulation {
  def InverterDelay: Int

  def AndGateDelay: Int

  def OrGateDelay: Int

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {
        output setSignal !inputSig
      }
    }

    input addAction invertAction
  }

  def andGate(in1: Wire, in2: Wire, output: Wire) = {
    def andAction() = {
      val in1Sid = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(AndGateDelay) {
        output setSignal (in1Sid & in2Sig)
      }
    }

    in1 addAction andAction
    in2 addAction andAction
  }

  /** Design orGate analogously to andGate */
  def orGate(in1: Wire, in2: Wire, output: Wire) = {
    def orAction() = {
      val in1Sid = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(OrGateDelay) {
        output setSignal (in1Sid | in2Sig)
      }
    }

    in1 addAction orAction
    in2 addAction orAction
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name  $currentTime new-value ${wire.getSignal}")
    }

    wire addAction probeAction
  }

  class Wire {
    private var sigVal = false
    private var action: List[Action] = List()

    def getSignal = sigVal

    def setSignal(s: Boolean) =
      if (s != sigVal) {
        sigVal = s
        action foreach (_ ())
      }

    def addAction(a: Action) = {
      action = a :: action
      a()
    }
  }
}
