package observatory

import Extraction.*
import java.time.LocalDate

trait ExtractionTest extends MilestoneSuite:
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  test("locationYearlyAverageRecords with empty iterable") {
    val inputSeq: Iterable[(LocalDate, Location, Temperature)] = IndexedSeq()
    val expectedSeq: Iterable[(Location, Temperature)] = IndexedSeq()
    assertEquals(locationYearlyAverageRecords(inputSeq), expectedSeq)
  }

  test("locationYearlyAverageRecords with one record") {
    val loc0 = Location(10.5, 11.6)
    val inputSeq: Iterable[(LocalDate, Location, Temperature)] = IndexedSeq(
      (LocalDate.of(2024, 9, 8), loc0, 2))
    val expectedSeq: Iterable[(Location, Temperature)] = IndexedSeq((loc0, 2))
    assertEquals(locationYearlyAverageRecords(inputSeq), expectedSeq)
  }

  test("locationYearlyAverageRecords one record per location, multiple locations") {
    val loc0 = Location(10.5, 11.6)
    val loc1 = Location(125.3, 145)
    val loc2 = Location(-35, 200)
    val inputSeq: Iterable[(LocalDate, Location, Temperature)] = IndexedSeq(
      (LocalDate.of(2024, 9, 8), loc0, 2),
      (LocalDate.of(2024, 3, 17), loc1, 5),
      (LocalDate.of(2024, 4, 1), loc2, 10))
    val expectedSeq: Iterable[(Location, Temperature)] = IndexedSeq(
      (loc2, 10), (loc0, 2), (loc1, 5))

    val result = locationYearlyAverageRecords(inputSeq).toList.sortWith((p1, p2) => p1._1.lat < p2._1.lat)
    assertEquals(result.size, 3)
    assertEquals(result, expectedSeq.toList)
  }

  test("locationYearlyAverageRecords multiple records per location, multiple locations") {
    val loc0 = Location(10.5, 11.6)
    val loc1 = Location(125.3, 145)
    val loc2 = Location(-35, 200)
    val inputSeq: Iterable[(LocalDate, Location, Temperature)] = IndexedSeq(
      (LocalDate.of(2024, 9, 8), loc0, 2),
      (LocalDate.of(2024, 3, 17), loc1, 5),
      (LocalDate.of(2024, 1, 1), loc0, 4),
      (LocalDate.of(2024, 7, 31), loc1, 12),
      (LocalDate.of(2024, 5, 29), loc1, 4),
      (LocalDate.of(2024, 4, 1), loc2, 10))
    val expectedSeq: Iterable[(Location, Temperature)] = IndexedSeq(
      (loc2, 10), (loc0, 3), (loc1, 7))

    val result = locationYearlyAverageRecords(inputSeq).toList.sortWith((p1, p2) => p1._1.lat < p2._1.lat)
    assertEquals(result.size, 3)
    assertEquals(result, expectedSeq.toList)
  }


