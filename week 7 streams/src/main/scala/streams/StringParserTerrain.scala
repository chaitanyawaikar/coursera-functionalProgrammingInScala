package streams

import common._

/**
 * This component implements a parser to define terrains from a
 * graphical ASCII representation.
 * 
 * When mixing in that component, a level can be defined by
 * defining the field `level` in the following form:
 * 
 *   val level =
 *     """------
 *       |--ST--
 *       |--oo--
 *       |--oo--
 *       |------""".stripMargin
 * 
 * - The `-` character denotes parts which are outside the terrain
 * - `o` denotes fields which are part of the terrain
 * - `S` denotes the start position of the block (which is also considered
     inside the terrain)
 * - `T` denotes the final position of the block (which is also considered
     inside the terrain)
 * 
 * In this example, the first and last lines could be omitted, and
 * also the columns that consist of `-` characters only.
 */
trait StringParserTerrain extends GameDef {

  /**
   * A ASCII representation of the terrain. This field should remain
   * abstract here.
   */
  val level: String

  /**
   * This method returns terrain function that represents the terrain
   * in `levelVector`. The vector contains parsed version of the `level`
   * string. For example, the following level
   * 
   *   val level =
   *     """ST
   *       |oo
   *       |oo""".stripMargin
   * 
   * is represented as
   * 
   *   Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
   *
   * The resulting function should return `true` if the position `pos` is
   * a valid position (not a '-' character) inside the terrain described
   * by `levelVector`.
   */
  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = {
    position: Pos => validPosition(levelVector, position)
  }
  
  def validPosition(levelVector: Vector[Vector[Char]], position: Pos) : Boolean = {

      def calculateValidPosition(levelVector: Vector[Vector[Char]], position: Pos, xAxis: Int): Boolean = {
         if (levelVector.isEmpty) false
         else {
           if (position.row == xAxis) validColumn(levelVector.head, position.col)
           else calculateValidPosition(levelVector.tail, position, xAxis+1)
         }

      }

      calculateValidPosition(levelVector, position, 0)
  }

  def validColumn(row: Vector[Char], yPosition: Int) : Boolean = {
    if (yPosition >= row.size || yPosition < 0) false
    else row(yPosition) match  {
      case ('o'|'S'|'T') => true
      case _ => false
    }
  }

  /**
   * This function should return the position of character `c` in the
   * terrain described by `levelVector`. You can assume that the `c`
   * appears exactly once in the terrain.
   *
   * Hint: you can use the functions `indexWhere` and / or `indexOf` of the
   * `Vector` class
   */
  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    def obtainPositionOfChar(c: Char, levelVector: Vector[Vector[Char]], xAxis: Int): Pos = {
         if (levelVector.isEmpty) new Pos(-1,-1)
         else {
           val yAxisOfChar = levelVector.head.indexOf(c,0)
           yAxisOfChar match {
             case x if x == -1 => obtainPositionOfChar(c, levelVector.tail, xAxis+1)
             case _  => new Pos(xAxis, yAxisOfChar)
            }
         }
    }
    obtainPositionOfChar(c, levelVector, 0)
  }

  private lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\n").map(str => Vector(str: _*)): _*)

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)

}
