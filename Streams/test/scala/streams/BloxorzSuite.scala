package streams

import Bloxorz.*

class BloxorzSuite extends munit.FunSuite:
  trait SolutionChecker extends GameDef with Solver with StringParserTerrain:
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    import Move.*
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
    }

  trait Level0 extends SolutionChecker {
    /* terrain for level 0 */

    val level =
      """Soo
        |-oo
        |Too""".stripMargin

    import Move.*

    val optSolution = List(Right, Down, Down, Left)
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    import Move.*

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 0") {
    new Level0 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(0, 2)), "0,2")
      assert(terrain(Pos(1, 2)), "1,2")
      assert(terrain(Pos(2, 0)), "2,0")
      assert(terrain(Pos(2, 2)), "2,2")
      assert(!terrain(Pos(1, -1)), "1,-1")
      assert(!terrain(Pos(-1, 2)), "-1,2")
      assert(!terrain(Pos(1, 3)), "1,3")
      assert(!terrain(Pos(3, 1)), "3,1")
      assert(!terrain(Pos(1, 0)), "1,0")
    }
  }

  test("terrain function level 1 (10pts)") {
    new Level1:
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }

  test("find char level 0") {
    new Level0 {
      assertEquals(startPos, Pos(0, 0))
      assertEquals(goal, Pos(2, 0))
    }
  }

  test("find char level 1 (10pts)") {
    new Level1:
      assertEquals(startPos, Pos(1, 1))
  }

  test("level 0 start block") {
    new Level0 {
      assert(startBlock.isStanding)
      assert(startBlock.isLegal)
    }
  }

  test("level 1 start block") {
    new Level1 {
      assert(startBlock.isStanding)
      assert(startBlock.isLegal)
    }
  }

  test("level 0 moves") {
    new Level0 {
      // start block is upright at 0,0
      val block : Block = startBlock

      // now block is not standing and in 0,1 and 0,2
      val right0 : Block = block.right
      assert(!right0.isStanding)
      assert(right0.isLegal)

      // block is still not standing but is in 1,1 and 1,2
      val down0 : Block = right0.down
      assert(!down0.isStanding)
      assert(down0.isLegal)

      // flips block up to 1,0 which is not a legal space
      val left0: Block = down0.left
      assert(!left0.isLegal)

      // rolls block to 2,1 and 2,2
      val down1 : Block = down0.down
      assert(!down1.isStanding)
      assert(down1.isLegal)

      // flips upright on target 2,0
      val left1 : Block = down1.left
      assert(left1.isStanding)
      assert(left1.isLegal)

      // flips upright on 0,3 which is not legal
      val right1 : Block = right0.right
      assert(!right1.isLegal)

      // rolls down to 3,1 and 3,2 which is not legal
      val down2 : Block = down1.down
      assert(!down2.isLegal)
    }
  }


  test("optimal solution for level 1 (5pts)") {
    new Level1:
      assertEquals(solve(solution), Block(goal, goal))
  }


  test("optimal solution length for level 1 (5pts)") {
    new Level1:
      assertEquals(solution.length, optsolution.length)
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
