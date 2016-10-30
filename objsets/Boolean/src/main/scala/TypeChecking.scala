/**
  * Created by mirceanedelcu on 10/6/16.
  */

abstract class IntSet {

}

class NonEmpty extends IntSet {

}

object Empty extends IntSet {

}

object demo {

  val a: Array[NonEmpty] = Array(new NonEmpty(1, Empty, Empty))

}