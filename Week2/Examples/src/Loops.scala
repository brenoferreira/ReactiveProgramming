/**
 * Created with IntelliJ IDEA.
 * User: brenocferreira
 * Date: 11/15/13
 * Time: 5:58 PM
 * To change this template use File | Settings | File Templates.
 */
object Loops {
  def While(condition: => Boolean)(command: () => Unit):Unit = {
    if(condition){
      command()
      While(condition)(command)
    }
    else ()
  }

  def Repeat(command: () => Unit)(condition: => Boolean):Unit = {
    command()
    if(condition) ()
    else Repeat(command)(condition)
  }

  def Repeat2(command: () => Unit) = {
    new {
      def Until(condition: => Boolean):Unit = {
        command()
        if(condition) ()
        else Until(condition)
      }
    }
  }
}
