package streams
import streams.GameDef
object Main extends GameDef {
  val pos1: Pos = new Pos(1,1)
  def goal = pos1
  def startPos = pos1
  def terrain = {_ => true}
  def main(args: Array[String]) = {
    val pos: Pos = new Pos(1,1)
    val b: Block = Block(pos,pos)
    val neighbors = b.neighbors
    println(neighbors)

  }

}
