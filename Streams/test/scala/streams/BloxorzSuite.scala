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

  test("level 1 moves") {
    new Level1 {
      val up0 : Block = startBlock.up
      assert(!up0.isLegal)

      val right0 : Block = startBlock.right
      assert(!right0.isStanding)
      assert(right0.isLegal)

      val left0 : Block = right0.left
      assert(left0.isStanding)
      assert(left0.isLegal)

      val right1 : Block = right0.right
      assert(right1.isStanding)
      assert(right1.isLegal)

      val down0 : Block = right1.down
      assert(!down0.isStanding)
      assert(down0.isLegal)

      val right2 : Block = down0.right
      assert(!right2.isStanding)
      assert(right2.isLegal)

      val right3 : Block = right2.right
      assert(!right3.isStanding)
      assert(right3.isLegal)

      val right4 : Block = right3.right
      assert(!right4.isStanding)
      assert(right4.isLegal)

      val up1 : Block = right4.up
      assert(!up1.isLegal)

      val down1 : Block = right4.down
      assert(down1.isStanding)
      assert(down1.isLegal)
    }
  }

  test("level 0 neighbors") {
    new Level0 {
      assertEquals(startBlock.neighbors,
        List((startBlock.left, Move.Left),
             (startBlock.up, Move.Up),
             (startBlock.right, Move.Right),
             (startBlock.down, Move.Down)))
      assertEquals(startBlock.legalNeighbors,
        List((startBlock.right, Move.Right)))

      val right0 : Block = startBlock.right
      assertEquals(right0.neighbors,
        List((right0.left, Move.Left),
             (right0.up, Move.Up),
             (right0.right, Move.Right),
             (right0.down, Move.Down)))
      assertEquals(right0.legalNeighbors,
        List((right0.left, Move.Left), (right0.down, Move.Down)))

      val down0 : Block = right0.down
      assertEquals(down0.neighbors,
        List((down0.left, Move.Left),
          (down0.up, Move.Up),
          (down0.right, Move.Right),
          (down0.down, Move.Down)))
      assertEquals(down0.legalNeighbors,
        List((down0.up, Move.Up), (down0.down, Move.Down)))

      val down1: Block = down0.down
      assertEquals(down1.neighbors,
        List((down1.left, Move.Left),
          (down1.up, Move.Up),
          (down1.right, Move.Right),
          (down1.down, Move.Down)))
      assertEquals(down1.legalNeighbors,
        List((down1.left, Move.Left), (down1.up, Move.Up)))
    }
  }

  test("level 1 neighbors") {
    new Level1 {
      assertEquals(startBlock.neighbors,
        List((startBlock.left, Move.Left),
             (startBlock.up, Move.Up),
             (startBlock.right, Move.Right),
             (startBlock.down, Move.Down)))
      assertEquals(startBlock.legalNeighbors,
        List((startBlock.right, Move.Right), (startBlock.down, Move.Down)))

      val right0 : Block = startBlock.right
      assertEquals(right0.neighbors,
        List((right0.left, Move.Left),
          (right0.up, Move.Up),
          (right0.right, Move.Right),
          (right0.down, Move.Down)))
      assertEquals(right0.legalNeighbors,
        List((right0.left, Move.Left),
             (right0.right, Move.Right),
             (right0.down, Move.Down)))

      val right1: Block = right0.right
      assertEquals(right1.neighbors,
        List((right1.left, Move.Left),
          (right1.up, Move.Up),
          (right1.right, Move.Right),
          (right1.down, Move.Down)))
      assertEquals(right1.legalNeighbors,
        List((right1.left, Move.Left), (right1.down, Move.Down)))

      val down0: Block = right1.down
      assertEquals(down0.neighbors,
        List((down0.left, Move.Left),
          (down0.up, Move.Up),
          (down0.right, Move.Right),
          (down0.down, Move.Down)))
      assertEquals(down0.legalNeighbors,
        List((down0.left, Move.Left),
          (down0.up, Move.Up),
          (down0.right, Move.Right)))

      val right2: Block = down0.right
      assertEquals(right2.neighbors,
        List((right2.left, Move.Left),
          (right2.up, Move.Up),
          (right2.right, Move.Right),
          (right2.down, Move.Down)))
      assertEquals(right2.legalNeighbors,
        List((right2.left, Move.Left),
          (right2.up, Move.Up),
          (right2.right, Move.Right),
          (right2.down, Move.Down)))

      val right3: Block = right2.right
      assertEquals(right3.neighbors,
        List((right3.left, Move.Left),
          (right3.up, Move.Up),
          (right3.right, Move.Right),
          (right3.down, Move.Down)))
      assertEquals(right3.legalNeighbors,
        List((right3.left, Move.Left),
          (right3.right, Move.Right),
          (right3.down, Move.Down)))

      val right4: Block = right3.right
      assertEquals(right4.neighbors,
        List((right4.left, Move.Left),
          (right4.up, Move.Up),
          (right4.right, Move.Right),
          (right4.down, Move.Down)))
      assertEquals(right4.legalNeighbors,
        List((right4.left, Move.Left),
          (right4.right, Move.Right),
          (right4.down, Move.Down)))
    }
  }

  test("level 0 done") {
    new Level0 {
      assert(!done(startBlock))

      val right0 : Block = startBlock.right
      assert(!done(right0))

      val down0 : Block = right0.down
      assert(!done(down0))

      val down1 : Block = down0.down
      assert(!done(down1))

      val left0 : Block = down1.left
      assert(done(left0))
    }
  }

  test("level 1 done") {
    new Level1 {
      assert(!done(startBlock))

      val right0 : Block = startBlock.right
      assert(!done(right0))

      val right1 : Block = right0.right
      assert(!done(right1))

      val down0 : Block = right1.down
      assert(!done(down0))

      val right2 : Block = down0.right
      assert(!done(right2))

      val right3 : Block = right2.right
      assert(!done(right3))

      // We now reverse the order of the last 2 moves so that the block lies
      // flat on the target, this should not be considered done as the block
      // is not standing
      val wrongDown : Block = right3.down
      assert(!done(wrongDown))
      val wrongRight : Block = wrongDown.right
      assert(!done(wrongRight))

      // We now do the correct order of moves to the finish
      val right4 : Block = right3.right
      assert(!done(right4))
      val down1 : Block = right4.down
      assert(done(down1))
    }
  }

  test("level 0 neighborsWithHistory") {
    new Level0 {
      val result0 = neighborsWithHistory(startBlock, List())
      val expected0 = Set(
        (Block(Pos(0, 1), Pos(0, 2)), List(Move.Right))
      ).to(LazyList)
      assertEquals(result0, expected0)

      val result1 = neighborsWithHistory(
        Block(Pos(0, 1), Pos(0, 2)), List(Move.Right))
      val expected1 = Set(
        (Block(Pos(0, 0), Pos(0, 0)), List(Move.Left, Move.Right)),
        (Block(Pos(1, 1), Pos(1, 2)), List(Move.Down, Move.Right))
      ).to(LazyList)
      assertEquals(result1, expected1)

      val result2 = neighborsWithHistory(
        Block(Pos(1, 1), Pos(1, 2)), List(Move.Down, Move.Right))
      val expected2 = Set(
        (Block(Pos(0, 1), Pos(0, 2)), List(Move.Up, Move.Down, Move.Right)),
        (Block(Pos(2, 1), Pos(2, 2)), List(Move.Down, Move.Down, Move.Right))
      ).to(LazyList)
      assertEquals(result2, expected2)
    }
  }

  test("level 1 neighborsWithHistory") {
    new Level1 {
      val result0 = neighborsWithHistory(startBlock, List(Move.Left, Move.Up))
      val expected0 = Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Move.Right, Move.Left, Move.Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Move.Down, Move.Left, Move.Up))
      ).to(LazyList)
      assertEquals(result0, expected0)

      val result1 = neighborsWithHistory(
        Block(Pos(1, 2), Pos(1, 3)), List(Move.Right))
      val expected1 = Set(
        (Block(Pos(1, 1), Pos(1, 1)), List(Move.Left, Move.Right)),
        (Block(Pos(1, 4), Pos(1, 4)), List(Move.Right, Move.Right)),
        (Block(Pos(2, 2), Pos(2, 3)), List(Move.Down, Move.Right))
      ).to(LazyList)
      assertEquals(result1, expected1)

      val result2 = neighborsWithHistory(
        Block(Pos(2, 5), Pos(3, 5)), List(Move.Right, Move.Down))
      val expected2 = Set(
        (Block(Pos(2, 4), Pos(3, 4)), List(Move.Left, Move.Right, Move.Down)),
        (Block(Pos(1, 5), Pos(1, 5)), List(Move.Up, Move.Right, Move.Down)),
        (Block(Pos(2, 6), Pos(3, 6)), List(Move.Right, Move.Right, Move.Down)),
        (Block(Pos(4, 5), Pos(4, 5)), List(Move.Down, Move.Right, Move.Down))
      ).to(LazyList)
      assertEquals(result2, expected2)
    }
  }

  test("new neighbors only level 0") {
    new Level0 {
      val neighbors0 = Set(
        (Block(Pos(0, 1), Pos(0, 2)), List(Move.Right))
      ).to(LazyList)
      val explored0 : Set[Block] = Set()
      assertEquals(newNeighborsOnly(neighbors0, explored0), neighbors0)

      val neighbors1 = Set(
        (Block(Pos(0, 0), Pos(0, 0)), List(Move.Left, Move.Right)),
        (Block(Pos(1, 1), Pos(1, 2)), List(Move.Down, Move.Right))
      ).to(LazyList)
      val explored1 = Set(Block(Pos(2, 2), Pos(2, 2)), Block(Pos(0, 2), Pos(1, 2)))
      assertEquals(newNeighborsOnly(neighbors1, explored1), neighbors1)

      val neighbors2 = Set(
        (Block(Pos(0, 1), Pos(0, 2)), List(Move.Up, Move.Down, Move.Right)),
        (Block(Pos(2, 1), Pos(2, 2)), List(Move.Down, Move.Down, Move.Right))
      ).to(LazyList)
      val explored2 = Set(
        Block(Pos(0, 0), Pos(0, 0)),
        Block(Pos(0, 1), Pos(0, 2)),
        Block(Pos(2, 1), Pos(2, 2)))
      val expected2 : LazyList[(Block, List[Move])] = LazyList()
      assertEquals(newNeighborsOnly(neighbors2, explored2), expected2)
    }
  }

  test("new neighbors only level 1") {
    new Level1 {
      val neighbors0 = Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Move.Right, Move.Left, Move.Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Move.Down, Move.Left, Move.Up))
      ).to(LazyList)
      val explored0 = Set(
        Block(Pos(1, 2), Pos(1, 3)),
        Block(Pos(1, 1), Pos(1, 1)))
      val expected0 = Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Move.Down, Move.Left, Move.Up))
      ).to(LazyList)
      assertEquals(newNeighborsOnly(neighbors0, explored0), expected0)

      val neighbors1 = Set(
        (Block(Pos(2, 4), Pos(3, 4)), List(Move.Left, Move.Right, Move.Down)),
        (Block(Pos(1, 5), Pos(1, 5)), List(Move.Up, Move.Right, Move.Down)),
        (Block(Pos(2, 6), Pos(3, 6)), List(Move.Right, Move.Right, Move.Down)),
        (Block(Pos(4, 5), Pos(4, 5)), List(Move.Down, Move.Right, Move.Down))
      ).to(LazyList)
      val explored1 = Set(
        Block(Pos(1, 5), Pos(1, 5)),
        Block(Pos(2, 6), Pos(3, 6)),
        Block(Pos(4, 5), Pos(4, 6)))
      val expected1 = Set(
        (Block(Pos(2, 4), Pos(3, 4)), List(Move.Left, Move.Right, Move.Down)),
        (Block(Pos(4, 5), Pos(4, 5)), List(Move.Down, Move.Right, Move.Down))
      ).to(LazyList)
      assertEquals(newNeighborsOnly(neighbors1, explored1), expected1)
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
