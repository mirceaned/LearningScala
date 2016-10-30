package idealized.scala

object operations {

  abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat
    def +(that: Nat): Nat
    def -(that: Nat): Nat
  }

  object Zero extends Nat {

    def isZero: Boolean = true

    override def predecessor: Nat = throw new NotImplementedError()
    override def successor: Nat = new Succ(Zero)

    override def +(that: Nat): Nat = that

    override def -(that: Nat): Nat = {
      if (that.isZero) Zero
      else throw new NotImplementedError()
    }
  }
  //done


  class Succ(n: Nat) extends Nat {

    def isZero: Boolean = false

    override def predecessor: Nat = {
      if (n.isZero) throw new NotImplementedError()
      else Zero /**/
    }

    override def successor: Nat = {

    }

    override def +(that: Nat): Nat = {
      // call successor on this as many times as that has a predecessor which is not Zero
      if (that.isZero) n
      else successor + that.predecessor // add 1
    }

    override def -(that: Nat): Nat = {
      //call predecessor on this as many times as that has a predecessor, throw exception is negative
      if (n.isZero) throw new NotImplementedError()
      else if (that.isZero) n
      else predecessor - that.predecessor // subtract 1
    }
  }


}



