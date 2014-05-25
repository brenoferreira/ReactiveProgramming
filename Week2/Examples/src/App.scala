/**
 * Created with IntelliJ IDEA.
 * User: brenocferreira
 * Date: 11/15/13
 * Time: 6:03 PM
 * To change this template use File | Settings | File Templates.
 */
object App {
  def main(args: Array[String]) {
    var i = 0
    var res = 1
    val pow = () => {
      i = i + 1
      res = res * 2
    }

    Loops.While(i < 5)(pow)

    println(res)

    i = 0
    res = 1

    Loops.Repeat(pow)(i >= 5)

    println(res)

    i = 0
    res = 1

    Loops Repeat2(pow) Until (i >= 5)

    println(res)
  }
}
