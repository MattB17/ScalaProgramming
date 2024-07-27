package barneshut

import java.util.concurrent.*
import scala.collection.*
import scala.math.*
import scala.collection.parallel.*
import barneshut.conctrees.ConcBuffer

class BarnesHutSuite extends munit.FunSuite:
  // test cases for quad tree

  import FloatOps.*
  test("Empty: center of mass should be the center of the cell") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  test("Empty: mass should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  test("Empty: total should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  test("Leaf with 1 body") {
    val b = Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))

    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test ("Leaf with multiple bodies") {
    val b0 = Body(100f, 18f, 26f, 0f, 0f)
    val b1 = Body(100f, 16f, 24f, 0f, 0f)
    val b2 = Body(100f, 20f, 28f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b0, b1, b2))

    assert(quad.mass ~= 300f, s"${quad.mass} should be 300f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 3, s"${quad.total} should be 3")
  }

  test("Fork with 4 empty quadrants") {
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 0f, s"${quad.mass} should be 0f")
    assert(quad.massX ~= 20f, s"${quad.massX} should be 0f")
    assert(quad.massY ~= 30f, s"${quad.massY} should be 0f")
    assert(quad.total == 0, s"${quad.total} should be 0")
  }


  test("Fork with 3 empty quadrants and 1 leaf (nw)") {
    val b = Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Fork with 3 empty quadrants and 1 leaf (ne)") {
    val b = Body(123f, 18f, 26f, 0f, 0f)
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Leaf(22.5f, 27.5f, 5f, Seq(b))
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Fork with 3 empty quadrants and 1 leaf (sw)") {
    val b = Body(123f, 18f, 26f, 0f, 0f)
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Leaf(17.5f, 32.5f, 5f, Seq(b))
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Fork with 3 empty quadrants and 1 leaf (se)") {
    val b = Body(123f, 18f, 26f, 0f, 0f)
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Leaf(22.5f, 32.5f, 5f, Seq(b))
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Fork with multiple leaves") {
    val b0 = Body(100f, 18f, 26f, 0f, 0f)
    val b1 = Body(100f, 16f, 24f, 0f, 0f)
    val b2 = Body(100f, 20f, 28f, 0f, 0f)
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Leaf(22.5f, 27.5f, 5f, Seq(b0))
    val sw = Leaf(17.5f, 32.5f, 5f, Seq(b1))
    val se = Leaf(22.5f, 32.5f, 5f, Seq(b2))

    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 300f, s"${quad.mass} should be 300f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 3, s"${quad.total} should be 3")
  }

  test("Empty.insert(b) should return a Leaf with only that body (2pts)") {
    val quad = Empty(51f, 46.3f, 5f)
    val b = Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
  }

  test("Leaf.insert(b) should return a Leaf when below minimumSize") {
    val b0 = Body(10f, 5f, 10f, 0f, 0f)
    val b1 = Body(5f, 5f, 10f, 1f, 1f)
    val quad = Leaf(5f, 10f, 0.000005f, Seq(b0))
    val inserted = quad.insert(b1)
    inserted match {
      case Leaf(centerX, centerY, size, bodies) => {
        assert(centerX == 5f, s"$centerX should be 5f")
        assert(centerY == 10f, s"$centerY should be 10f")
        assert(size == 0.000005f, s"$size should be 0.000005f")
        assert(bodies == Seq(b0, b1), s"$bodies should contain 2 elements")
      }
      case _ => fail("Leaf.insert() should return another Leaf")
    }
  }

  test("Leaf.insert(b) should return a Fork when above minimumSize") {
    val b0 = Body(10f, 2.5f, 2.5f, 0f, 0f)
    val b1 = Body(5f, 7.5f, 7.5f, 0f, 0f)
    val quad = Leaf(5f, 5f, 10f, Seq(b0))
    val inserted = quad.insert(b1)
    inserted match {
      case Fork(nw, ne, sw, se) => {
        nw match {
          case Leaf(centerX, centerY, size, bodies) => {
            assert(centerX == 2.5f)
            assert(centerY == 2.5f)
            assert(size == 5f)
            assert(bodies == Seq(b0), "nw should contain only b0")
          }
          case _ => fail("nw should be a Leaf")
        }
        ne match {
          case Empty(centerX, centerY, size) => {
            assert(centerX == 7.5f)
            assert(centerY == 2.5f)
            assert(size == 5f)
          }
          case _ => fail("ne should be Empty")
        }
        sw match {
          case Empty(centerX, centerY, size) => {
            assert(centerX == 2.5f)
            assert(centerY == 7.5f)
            assert(size == 5f)
          }
          case _ => fail("sw should be Empty")
        }
        se match {
          case Leaf(centerX, centerY, size, bodies) => {
            assert(centerX == 7.5f)
            assert(centerY == 7.5f)
            assert(size == 5f)
            assert(bodies == Seq(b1), "nw should contain only b0")
          }
          case _ => fail("se should be a Leaf")
        }
      }
      case _ => fail("Leaf.insert() should return a Fork")
    }
  }

  test("Fork insert") {
    val b0 = Body(10f, 2.5f, 2.5f, 0f, 0f)
    val b1 = Body(5f, 7.5f, 7.5f, 0f, 0f)
    val currNw = Leaf(2.5f, 2.5f, 5f, Seq(b0))
    val currNe = Empty(7.5f, 2.5f, 5f)
    val currSw = Empty(2.5f, 7.5f, 5f)
    val currSe = Empty(7.5f, 7.5f, 5f)
    val quad = Fork(currNw, currNe, currSw, currSe)
    val inserted = quad.insert(b1)
    inserted match {
      case Fork(nw, ne, sw, se) => {
        nw match {
          case Leaf(centerX, centerY, size, bodies) => {
            assert(centerX == 2.5f)
            assert(centerY == 2.5f)
            assert(size == 5f)
            assert(bodies == Seq(b0), "nw should contain only b0")
          }
          case _ => fail("nw should still be a Leaf")
        }
        ne match {
          case Empty(centerX, centerY, size) => {
            assert(centerX == 7.5f)
            assert(centerY == 2.5f)
            assert(size == 5f)
          }
          case _ => fail("ne should still be Empty")
        }
        sw match {
          case Empty(centerX, centerY, size) => {
            assert(centerX == 2.5f)
            assert(centerY == 7.5f)
            assert(size == 5f)
          }
          case _ => fail("sw should still be Empty")
        }
        se match {
          case Leaf(centerX, centerY, size, bodies) => {
            assert(centerX == 7.5f)
            assert(centerY == 7.5f)
            assert(size == 5f)
            assert(bodies == Seq(b1), "nw should contain only b0")
          }
          case _ => fail("se should be a Leaf")
        }
      }
      case _ => fail("Inserting into a Fork should yield a Fork")
    }
  }

  // test cases for Body

  test("Body.updated should do nothing for Empty quad trees") {
    val b1 = Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    assertEquals(body.xspeed, 0f, precisionThreshold)
    assertEquals(body.yspeed, 0f, precisionThreshold)
  }

  test("Body.updated should take bodies in a Leaf into account (2pts)") {
    val b1 = Body(123f, 18f, 26f, 0f, 0f)
    val b2 = Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    assert(body.xspeed ~= 12.587037f)
    assert(body.yspeed ~= 0.015557117f)
  }

  test("Body.updated with a distant Fork") {
    val b0 = Body(77f, 8f, 26f, 0f, 0f)
    val b1 = Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b1))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)
    val body = b0.updated(quad)

    assert(body.mass == 77f)
    assert(body.x == 8f)
    assert(body.y == 26f)
    assert(body.xspeed ~= 1.230000f)
    assert(body.yspeed == 0f)
  }

  test("Body.updated with a close Fork") {
    val b0 = Body(10f, 5f, 5f, 1f, 1f)
    val b1 = Body(10f, 2f, 1f, 2f, 2f)
    val b2 = Body(5f, 11f, 5f, 3f, 3f)

    val nw = Leaf(2.5f, 2.5f, 5f, Seq(b1))
    val ne = Empty(7.5f, 2.5f, 5f)
    val sw = Empty(2.5f, 7.5f, 5f)
    val se = Leaf(7.5f, 7.5f, 5f, Seq(b2))

    val quad = Fork(nw, ne, sw, se)

    val body = b0.updated(quad)

    assert(body.mass == 10f)
    assert(body.x ~= 5.01f)
    assert(body.y ~= 5.01f)
    assert(body.xspeed ~= 0.89888888f)
    assert(body.yspeed ~= 0.68f)
  }

  // test cases for sector matrix

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96 (2pts)") {
    val body = Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).find(_ == body).isDefined
    assert(res, s"Body not found in the right sector")
  }

  test("'SectorMatrix.+=' adding bodies on boundaries") {
    val b0 = Body(5, 1, 1, 0.1f, 0.1f)
    val b1 = Body(10, 80, 80, 0.3f, 0.2f)
    val b2 = Body(7, 1, 80, 0.1f, 0.1f)
    val b3 = Body(15, 80, 1, 0f, 0f)
    val boundaries = Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 81
    boundaries.maxY = 81
    val sm = SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += b0
    sm += b1
    sm += b2
    sm +=b3

    val res0 = sm(0, 0).size == 1 && sm(0, 0).find(_ == b0).isDefined
    assert(res0, "b0 not found in the right sector")

    val res1 = sm(7, 7).size == 1 && sm(7, 7).find(_ == b1).isDefined
    assert(res1, "b1 not found in the right sector")

    val res2 = sm(0, 7).size == 1 && sm(0, 7).find(_ == b2).isDefined
    assert(res2, "b2 not found in the right sector")

    val res3 = sm(7, 0).size == 1 && sm(7, 0).find(_ == b3).isDefined
    assert(res3, "b3 not found in the right sector")
  }

  test("'SectorMatrix.+=' adding bodies outside boundaries") {
    val b0 = Body(5, 1, 1, 0.1f, 0.1f)
    val b1 = Body(10, 100, 105, 0.3f, 0.2f)
    val b2 = Body(7, 6, 98, 0.1f, 0.1f)
    val b3 = Body(15, 120, 4, 0f, 0f)
    val boundaries = Boundaries()
    boundaries.minX = 11
    boundaries.minY = 11
    boundaries.maxX = 91
    boundaries.maxY = 91
    val sm = SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += b0
    sm += b1
    sm += b2
    sm += b3

    val res0 = sm(0, 0).size == 1 && sm(0, 0).find(_ == b0).isDefined
    assert(res0, "b0 not found in the right sector")

    val res1 = sm(7, 7).size == 1 && sm(7, 7).find(_ == b1).isDefined
    assert(res1, "b1 not found in the right sector")

    val res2 = sm(0, 7).size == 1 && sm(0, 7).find(_ == b2).isDefined
    assert(res2, "b2 not found in the right sector")

    val res3 = sm(7, 0).size == 1 && sm(7, 0).find(_ == b3).isDefined
    assert(res3, "b3 not found in the right sector")
  }

  test("'SectorMatrix.+=' adding bodies to the same sector") {
    val b0 = Body(5, 31, 45, 0f, 0f)
    val b1 = Body(10, 33, 41, 0.5f, 0.4f)
    val b2 = Body(12, 39, 47, 0.2f, 0.9f)
    val boundaries = Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 81
    boundaries.maxY = 81
    val sm = SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += b0
    sm += b1
    sm += b2

    val res = sm(3, 4).size == 3
    assert(res, "At least one body added to wrong sector")
  }

  test("'combine' with two SectorMatrices that have elements at boundaries") {
    val b0 = Body(5, 1, 1, 0.1f, 0.1f)
    val b1 = Body(10, 80, 80, 0.3f, 0.2f)
    val b2 = Body(7, 1, 80, 0.1f, 0.1f)
    val b3 = Body(15, 80, 1, 0f, 0f)
    val boundaries = Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 81
    boundaries.maxY = 81
    val sm1 = SectorMatrix(boundaries, SECTOR_PRECISION)
    sm1 += b0
    sm1 += b1
    sm1 += b2
    sm1 += b3

    val b4 = Body(5, 0, 0, 0.1f, 0.1f)
    val b5 = Body(10, 100, 105, 0.3f, 0.2f)
    val b6 = Body(7, 1, 98, 0.1f, 0.1f)
    val b7 = Body(15, 120, 1, 0f, 0f)
    val sm2 = SectorMatrix(boundaries, SECTOR_PRECISION)
    sm2 += b4
    sm2 += b5
    sm2 += b6
    sm2 += b7

    val sm = sm1.combine(sm2)

    val res0 = sm(0, 0).size == 2
    assert(res0, "b0 not found in the right sector")

    val res1 = sm(7, 7).size == 2
    assert(res1, "b1 not found in the right sector")

    val res2 = sm(0, 7).size == 2
    assert(res2, "b2 not found in the right sector")

    val res3 = sm(7, 0).size == 2
    assert(res3, "b3 not found in the right sector")
  }

  test("'combine' with two disjoint sector matrices") {
    val b0 = Body(5, 1, 1, 0.1f, 0.1f)
    val b1 = Body(10, 80, 80, 0.3f, 0.2f)
    val b2 = Body(7, 1, 80, 0.1f, 0.1f)
    val b3 = Body(15, 80, 1, 0f, 0f)
    val boundaries = Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 81
    boundaries.maxY = 81
    val sm1 = SectorMatrix(boundaries, SECTOR_PRECISION)
    sm1 += b0
    sm1 += b1
    sm1 += b2
    sm1 += b3

    val b4 = Body(5, 31, 45, 0f, 0f)
    val b5 = Body(10, 33, 41, 0.5f, 0.4f)
    val b6 = Body(12, 39, 47, 0.2f, 0.9f)
    val sm2 = SectorMatrix(boundaries, SECTOR_PRECISION)
    sm2 += b4
    sm2 += b5
    sm2 += b6

    val sm = sm1.combine(sm2)

    val res0 = sm(0, 0).size == 1 && sm(0, 0).find(_ == b0).isDefined
    assert(res0, "b0 not found in the right sector")

    val res1 = sm(7, 7).size == 1 && sm(7, 7).find(_ == b1).isDefined
    assert(res1, "b1 not found in the right sector")

    val res2 = sm(0, 7).size == 1 && sm(0, 7).find(_ == b2).isDefined
    assert(res2, "b2 not found in the right sector")

    val res3 = sm(7, 0).size == 1 && sm(7, 0).find(_ == b3).isDefined
    assert(res3, "b3 not found in the right sector")

    val res = sm(3, 4).size == 3
    assert(res, "At least one body added to wrong sector")
  }

  test("Simulation update boundaries body inside boundary") {
    val b = Body(10f, 5f, 5f, 3f, 3f)
    val boundaries = Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 10
    boundaries.maxY = 10

    val model = SimulationModel()
    val sim = Simulator(model.taskSupport, model.timeStats)
    val newBoundaries = sim.updateBoundaries(boundaries, b)

    assert(newBoundaries.minX == boundaries.minX)
    assert(newBoundaries.minY == boundaries.minY)
    assert(newBoundaries.maxX == boundaries.maxX)
    assert(newBoundaries.maxY == boundaries.maxY)
  }

  test("Simulation update boundaries body left of boundary") {
    val b = Body(10f, 5f, 5f, 3f, 3f)
    val boundaries = Boundaries()
    boundaries.minX = 10
    boundaries.minY = 1
    boundaries.maxX = 20
    boundaries.maxY = 10

    val model = SimulationModel()
    val sim = Simulator(model.taskSupport, model.timeStats)
    val newBoundaries = sim.updateBoundaries(boundaries, b)

    assert(newBoundaries.minX == 5f)
    assert(newBoundaries.minY == boundaries.minY)
    assert(newBoundaries.maxX == boundaries.maxX)
    assert(newBoundaries.maxY == boundaries.maxY)
  }

  test("Simulation update boundaries body right of boundary") {
    val b = Body(10f, 15f, 15f, 3f, 3f)
    val boundaries = Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 10
    boundaries.maxY = 10

    val model = SimulationModel()
    val sim = Simulator(model.taskSupport, model.timeStats)
    val newBoundaries = sim.updateBoundaries(boundaries, b)

    assert(newBoundaries.minX == boundaries.minX)
    assert(newBoundaries.minY == boundaries.minY)
    assert(newBoundaries.maxX == 15f)
    assert(newBoundaries.maxY == 15f)
  }

  test("Simulation update boundaries body below boundary") {
    val b = Body(10f, 5f, 20f, 3f, 3f)
    val boundaries = Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 10
    boundaries.maxY = 10

    val model = SimulationModel()
    val sim = Simulator(model.taskSupport, model.timeStats)
    val newBoundaries = sim.updateBoundaries(boundaries, b)

    assert(newBoundaries.minX == boundaries.minX)
    assert(newBoundaries.minY == boundaries.minY)
    assert(newBoundaries.maxX == boundaries.maxX)
    assert(newBoundaries.maxY == 20f)
  }

  test("Simulation mergeBoundaries with 2 identical boundaries") {
    val a = Boundaries()
    a.minX = 1
    a.maxX = 10
    a.minY = 1
    a.maxY = 10

    val b = Boundaries()
    b.minX = 1
    b.maxX = 10
    b.minY = 1
    b.maxY = 10

    val model = SimulationModel()
    val sim = Simulator(model.taskSupport, model.timeStats)
    val result = sim.mergeBoundaries(a, b)

    assert(result.minX == a.minX)
    assert(result.minY == a.minY)
    assert(result.maxX == a.maxX)
    assert(result.maxY == a.maxY)
  }

  test("Simulation mergeBoundaries one boundaries contained in another") {
    val a = Boundaries()
    a.minX = 11
    a.maxX = 20
    a.minY = 11
    a.maxY = 20

    val b = Boundaries()
    b.minX = 1
    b.maxX = 30
    b.minY = 1
    b.maxY = 30

    val model = SimulationModel()
    val sim = Simulator(model.taskSupport, model.timeStats)
    val result = sim.mergeBoundaries(a, b)

    assert(result.minX == b.minX)
    assert(result.minY == b.minY)
    assert(result.maxX == b.maxX)
    assert(result.maxY == b.maxY)
  }

  test("Simulation mergeBoundaries 2 boundaries with overlap") {
    val a = Boundaries()
    a.minX = 1
    a.maxX = 20
    a.minY = 1
    a.maxY = 20

    val b = Boundaries()
    b.minX = 11
    b.maxX = 30
    b.minY = 11
    b.maxY = 30

    val model = SimulationModel()
    val sim = Simulator(model.taskSupport, model.timeStats)
    val result = sim.mergeBoundaries(a, b)

    assert(result.minX == a.minX)
    assert(result.minY == a.minY)
    assert(result.maxX == b.maxX)
    assert(result.maxY == b.maxY)
  }

  test("Simulation mergeBoundaries with 2 disjoint boundaries") {
    val a = Boundaries()
    a.minX = 1
    a.maxX = 10
    a.minY = 1
    a.maxY = 10

    val b = Boundaries()
    b.minX = 21
    b.maxX = 40
    b.minY = 5
    b.maxY = 20

    val model = SimulationModel()
    val sim = Simulator(model.taskSupport, model.timeStats)
    val result = sim.mergeBoundaries(a, b)

    assert(result.minX == a.minX)
    assert(result.minY == a.minY)
    assert(result.maxX == b.maxX)
    assert(result.maxY == b.maxY)
  }

  test("Simulator computeSectorMatrix with no bodies") {
    val boundaries = Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 10
    boundaries.maxY = 10

    val model = SimulationModel()
    val sim = Simulator(model.taskSupport, model.timeStats)
    val result = sim.computeSectorMatrix(IndexedSeq(), boundaries)

    assert(result.boundaries.minX == 1)
    assert(result.boundaries.minY == 1)
    assert(result.boundaries.maxX == 10)
    assert(result.boundaries.maxY == 10)
  }

  test("Simulator computeSectorMatrix with multiple bodies") {
    val b0 = Body(5, 1, 1, 0.1f, 0.1f)
    val b1 = Body(10, 80, 80, 0.3f, 0.2f)
    val b2 = Body(7, 1, 80, 0.1f, 0.1f)
    val b3 = Body(15, 80, 1, 0f, 0f)
    val boundaries = Boundaries()
    boundaries.minX = 21
    boundaries.minY = 21
    boundaries.maxX = 40
    boundaries.maxY = 40

    val model = SimulationModel()
    val sim = Simulator(model.taskSupport, model.timeStats)
    val result = sim.computeSectorMatrix(IndexedSeq(b0, b1, b2, b3), boundaries)

    assert(result.boundaries.minX == 1)
    assert(result.boundaries.minY == 1)
    assert(result.boundaries.maxX == 80)
    assert(result.boundaries.maxY == 80)

    val res0 = result(0, 0).size == 1 && result(0, 0).find(_ == b0).isDefined
    assert(res0, "b0 not found in the right sector")

    val res1 = result(7, 7).size == 1 && result(7, 7).find(_ == b1).isDefined
    assert(res1, "b1 not found in the right sector")

    val res2 = result(0, 7).size == 1 && result(0, 7).find(_ == b2).isDefined
    assert(res2, "b2 not found in the right sector")

    val res3 = result(7, 0).size == 1 && result(7, 0).find(_ == b3).isDefined
    assert(res3, "b3 not found in the right sector")
  }

  test("Simulator updateBodies with no bodies") {
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    val model = SimulationModel()
    val sim = Simulator(model.taskSupport, model.timeStats)
    val result = sim.updateBodies(IndexedSeq(), quad)

    assert(result == IndexedSeq())
  }

  test("Simulator updateBodies with 1 body") {
    val b0 = Body(10f, 5f, 5f, 1f, 1f)
    val b1 = Body(10f, 2f, 1f, 2f, 2f)
    val b2 = Body(5f, 11f, 5f, 3f, 3f)

    val nw = Leaf(2.5f, 2.5f, 5f, Seq(b1))
    val ne = Empty(7.5f, 2.5f, 5f)
    val sw = Empty(2.5f, 7.5f, 5f)
    val se = Leaf(7.5f, 7.5f, 5f, Seq(b2))

    val quad = Fork(nw, ne, sw, se)

    val model = SimulationModel()
    val sim = Simulator(model.taskSupport, model.timeStats)
    val result = sim.updateBodies(IndexedSeq(b0), quad)

    assert(result.length == 1)
    assert(result.head.mass == 10f)
    assert(result.head.x ~= 5.01f)
    assert(result.head.y ~= 5.01f)
    assert(result.head.xspeed ~= 0.89888888f)
    assert(result.head.yspeed ~= 0.68f)
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds

object FloatOps:
  val precisionThreshold = 1e-4

  /** Floating comparison: assert(float ~= 1.7f). */
  extension (self: Float) def ~=(that: Float): Boolean =
    abs(self - that) < precisionThreshold

  /** Long floating comparison: assert(double ~= 1.7). */
  extension (self: Double) def ~=(that: Double): Boolean =
    abs(self - that) < precisionThreshold

  /** Floating sequences comparison: assert(floatSeq ~= Seq(0.5f, 1.7f). */
  extension (self: Seq[Float]) def ~=(that: Seq[Float]): Boolean =
    self.size == that.size &&
      self.zip(that).forall { case (a, b) =>
        abs(a - b) < precisionThreshold
      }

