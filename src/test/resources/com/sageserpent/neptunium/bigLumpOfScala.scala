package com.sageserpent.plutonium

import java.time.Instant
import java.time.temporal.ChronoUnit

import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryo.{Kryo, KryoException, KryoSerializable}
import com.sageserpent.americium
import com.sageserpent.americium._
import com.sageserpent.americium.randomEnrichment._
import com.sageserpent.americium.seqEnrichment._
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.{Gen, Prop}
import org.scalatest.exceptions.TestFailedException
import org.scalatest.prop.Checkers
import org.scalatest.{FlatSpec, Matchers}
import scalaz.std.stream
import scalaz.syntax.applicativePlus._

import scala.collection.immutable
import scala.collection.immutable.{::, TreeMap}
import scala.util.Random
import scala.reflect.runtime.universe.TypeTag

trait WorldBehaviours
  extends FlatSpec
    with Matchers
    with Checkers
    with WorldSpecSupport { this: WorldResource =>

  case class NonExistentId() {
    fail("If I am not supposed to exist, why is something asking for me?")
  }

  abstract class NonExistentHistory extends History {
    override type Id = NonExistentId
  }

  def worldWithNoHistoryBehaviour = {
    it should "not contain any identifiables" in {
      val scopeGenerator = for {
        when          <- unboundedInstantGenerator
        asOf          <- instantGenerator
        worldResource <- worldResourceGenerator
      } yield
        worldResource.acquireAndGet(world =>
          world.scopeFor(when = when, asOf = asOf))
      check(Prop.forAllNoShrink(scopeGenerator)((scope: Scope) => {
        val exampleBitemporal = Bitemporal.wildcard[NonExistentHistory]()

        scope.render(exampleBitemporal).isEmpty
      }))
    }

    it should "have no current revision" in {
      check(Prop.forAllNoShrink(worldResourceGenerator)(worldResource =>
        worldResource acquireAndGet { world =>
          (World.initialRevision == world.nextRevision) :| s"Initial revision of a world ${world.nextRevision} should be: ${World.initialRevision}."
        }))
    }
  }

  val faultyRecordingsGroupedByIdGenerator =
    mixedRecordingsGroupedByIdGenerator(faulty = true,
      forbidAnnihilations = false)

  def eventWhenFrom(recording: ((Unbounded[Instant], Event), Int)) =
    recording match {
      case ((eventWhen, _), _) => eventWhen
    }

  val chunksShareTheSameEventWhens: (((Unbounded[Instant], Unbounded[Instant]),
    Instant),
    ((americium.Unbounded[Instant],
      Unbounded[Instant]),
      Instant)) => Boolean = {
    case (((_, trailingEventWhen), _), ((leadingEventWhen, _), _)) => true
  }

  def worldWithHistoryAddedInOrderOfIncreasingEventTimeBehaviour = {
    it should "reveal all history up to the 'asOf' limit of a scope made from it" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigHistoryOverLotsOfThingsSortedInEventWhenOrder = random
          .splitIntoNonEmptyPieces(
            (recordingsGroupedById.flatMap(_.events) sortBy {
              case (eventWhen, _) => eventWhen
            }).zipWithIndex)
        asOfs <- Gen.listOfN(
          bigHistoryOverLotsOfThingsSortedInEventWhenOrder.length,
          instantGenerator) map (_.sorted)
        asOfToLatestEventWhenMap = TreeMap(
          asOfs zip (bigHistoryOverLotsOfThingsSortedInEventWhenOrder map (_.last) map eventWhenFrom): _*)
        chunksForRevisions = bigHistoryOverLotsOfThingsSortedInEventWhenOrder map (
          recordingAndEventIdPairs =>
            eventWhenFrom(recordingAndEventIdPairs.head) -> eventWhenFrom(
              recordingAndEventIdPairs.last)) zip asOfs
        latestAsOfsThatMapUnambiguouslyToEventWhens = chunksForRevisions
          .groupWhile(chunksShareTheSameEventWhens) map (_.last._2)
        latestEventWhenForEarliestAsOf = asOfToLatestEventWhenMap(
          latestAsOfsThatMapUnambiguouslyToEventWhens.head)
        queryWhen <- latestEventWhenForEarliestAsOf match {
          case NegativeInfinity() => instantGenerator
          case PositiveInfinity() => Gen.fail
          case Finite(latestDefiniteEventWhenForEarliestAsOf) =>
            Gen.frequency(
              3 -> (Gen
                .posNum[Long] map (latestDefiniteEventWhenForEarliestAsOf
                .plusSeconds(_))),
              1 -> Gen.const(latestDefiniteEventWhenForEarliestAsOf))
        }
        asOfsIncludingAllEventsNoLaterThanTheQueryWhen = latestAsOfsThatMapUnambiguouslyToEventWhens takeWhile (
          asOf => asOfToLatestEventWhenMap(asOf) <= Finite(queryWhen))
      } yield
        (worldResource,
          recordingsGroupedById,
          bigHistoryOverLotsOfThingsSortedInEventWhenOrder,
          asOfs,
          queryWhen,
          asOfToLatestEventWhenMap,
          asOfsIncludingAllEventsNoLaterThanTheQueryWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        recordingsGroupedById,
        bigHistoryOverLotsOfThingsSortedInEventWhenOrder,
        asOfs,
        queryWhen,
        asOfToLatestEventWhenMap,
        asOfsIncludingAllEventsNoLaterThanTheQueryWhen) =>
          worldResource acquireAndGet { world =>
            recordEventsInWorld(
              liftRecordings(bigHistoryOverLotsOfThingsSortedInEventWhenOrder),
              asOfs,
              world)

            assert(asOfsIncludingAllEventsNoLaterThanTheQueryWhen.nonEmpty)

            val checks = for {
              asOf <- asOfsIncludingAllEventsNoLaterThanTheQueryWhen
              scope                    = world.scopeFor(Finite(queryWhen), asOf)
              eventWhenAlignedWithAsOf = asOfToLatestEventWhenMap(asOf)
              RecordingsNoLaterThan(
              historyId,
              historiesFrom,
              pertinentRecordings,
              _,
              _) <- recordingsGroupedById flatMap (_.thePartNoLaterThan(
                implicitly[Ordering[Unbounded[Instant]]]
                  .min(Finite(queryWhen), eventWhenAlignedWithAsOf)))
              Seq(history) = {
                assert(pertinentRecordings.nonEmpty)
                historiesFrom(scope)
              }
            } yield (historyId, history.datums, pertinentRecordings.map(_._1))

            if (checks.nonEmpty)
              Prop.all(checks.map {
                case (historyId, actualHistory, expectedHistory) =>
                  ((actualHistory.length == expectedHistory.length) :| s"For ${historyId}, got: ${actualHistory.length} datums, but expected: ${expectedHistory.length}, actual history: $actualHistory, expected history: $expectedHistory.") &&
                    Prop.all(
                      (actualHistory zip expectedHistory zipWithIndex) map {
                        case ((actual, expected), step) =>
                          (actual == expected) :| s"For ${historyId}, @step ${step}, ${actual} was expected to be: ${expected}"
                      }: _*)
              }: _*)
            else Prop.undecided
          }
      })
    }
  }

  def worldBehaviour = {
    it should "not mysteriously fail to yield items" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        referringHistoryRecordingsGroupedById <- referringHistoryRecordingsGroupedByIdGenerator(
          forbidMeasurements = true)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            referringHistoryRecordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          referringHistoryRecordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen)
      check(
        Prop.forAllNoShrink(testCaseGenerator) {
          case (worldResource,
          referringHistoryRecordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen) =>
            worldResource acquireAndGet {
              world =>
                recordEventsInWorld(
                  liftRecordings(bigShuffledHistoryOverLotsOfThings),
                  asOfs,
                  world)

                val scope = world.scopeFor(queryWhen, world.nextRevision)

                val checks = (for {
                  RecordingsNoLaterThan(
                  referringHistoryId,
                  referringHistoriesFrom,
                  _,
                  _,
                  _) <- referringHistoryRecordingsGroupedById flatMap (_.thePartNoLaterThan(
                    queryWhen))
                } yield referringHistoryId -> referringHistoriesFrom(scope))

                if (checks.nonEmpty)
                  Prop.all(checks map {
                    case (id, itemSingletonSequence) =>
                      (1 == itemSingletonSequence.size) :| s"Expected there to be a single item for id: $id."
                  }: _*)
                else Prop.undecided
            }
        },
        MinSuccessful(300)
      )
    }

    it should "deduce the most accurate type for items based on the events that refer to them" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        seed          <- seedGenerator
        random = new Random(seed)
        fooHistoryIds <- Gen.nonEmptyContainerOf[Set, FooHistory#Id](
          fooHistoryIdGenerator)
        numberOfReferrers <- Gen.chooseNum(1, 4)
        referringHistoryIds: Set[ReferringHistory#Id] <- Gen
          .containerOfN[Set, ReferringHistory#Id](numberOfReferrers,
          referringHistoryIdGenerator)
        if (referringHistoryIds intersect fooHistoryIds).isEmpty
      } yield (worldResource, random, fooHistoryIds, referringHistoryIds)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource, random, fooHistoryIds, referringHistoryIds) =>
          val sharedAsOf = Instant.ofEpochSecond(0L)
          worldResource acquireAndGet {
            world =>
              val linearizationIndices = fooHistoryIds zip Stream.continually {
                random.chooseAnyNumberFromZeroToOneLessThan(3)
              } toMap

            {
              val events = (for {
                fooHistoryId <- fooHistoryIds
                selectedReferringHistoryIds <- random.chooseSeveralOf(
                  referringHistoryIds,
                  random.chooseAnyNumberFromOneTo(referringHistoryIds.size))
              } yield {
                def referTo[AHistory <: History: TypeTag](
                                                           referringHistoryId: ReferringHistory#Id) =
                  Change.forTwoItems[ReferringHistory, AHistory](
                    referringHistoryId,
                    fooHistoryId, {
                      (referringHistory: ReferringHistory,
                       history: AHistory) =>
                        referringHistory.referTo(history)
                    })

                val waysOfReferringToAFooHistory
                : Array[ReferringHistory#Id => Change] =
                  Array(
                    referTo[History] _,
                    referTo[FooHistory] _,
                    referTo[MoreSpecificFooHistory] _) take (1 + linearizationIndices(
                    fooHistoryId))

                val eventConstructors =
                  waysOfReferringToAFooHistory.last :: List.fill(
                    referringHistoryIds.size - 1)(
                    random.chooseOneOf(waysOfReferringToAFooHistory))

                random
                  .shuffle(eventConstructors) zip referringHistoryIds map {
                  case (eventConstructor, referringHistoryId) =>
                    eventConstructor(referringHistoryId)
                }
              }).flatten

              for ((event, eventId) <- events zipWithIndex) {
                world.revise(eventId, event, sharedAsOf)
              }
            }

              val scope =
                world.scopeFor(NegativeInfinity[Instant](), sharedAsOf)

              Prop.all(fooHistoryIds.toSeq map {
                fooHistoryId =>
                  def fetch[AHistory <: History: TypeTag] =
                    scope.render(Bitemporal.withId[AHistory](fooHistoryId))
                  val waysOfFetchingHistory =
                    Array(fetch[History],
                      fetch[FooHistory],
                      fetch[MoreSpecificFooHistory])
                  val Seq(bitemporalWithExpectedFlavourOfHistory) =
                    waysOfFetchingHistory(linearizationIndices(fooHistoryId))
                  (bitemporalWithExpectedFlavourOfHistory.id == fooHistoryId) :| s"Expected to have a single bitemporal of id: $fooHistoryId, but got one of id: ${bitemporalWithExpectedFlavourOfHistory.id}"
              }: _*)
          }
      })
    }

    it should "reveal the same lack of history from a scope with an 'asOf' limit that comes at or after that revision but before the following revision" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen,
          random)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        recordingsGroupedById,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        queryWhen,
        random) =>
          worldResource acquireAndGet {
            world =>
              val revisions = recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                asOfs,
                world)

              val asOfComingAfterTheLastRevision = asOfs.last.plusSeconds(10L)

              val asOfPairs = asOfs
                .scanRight((asOfComingAfterTheLastRevision,
                  asOfComingAfterTheLastRevision)) {
                  case (asOf, (laterAsOf, _)) => (asOf, laterAsOf)
                } init

              val asOfsAndSharedRevisionTriples = (for {
                ((earlierAsOfCorrespondingToRevision,
                laterAsOfComingNoLaterThanAnySucceedingRevision),
                revision) <- asOfPairs zip revisions
                laterAsOfSharingTheSameRevisionAsTheEarlierOne = earlierAsOfCorrespondingToRevision plusSeconds random
                  .chooseAnyNumberFromZeroToOneLessThan(
                    earlierAsOfCorrespondingToRevision.until(
                      laterAsOfComingNoLaterThanAnySucceedingRevision,
                      ChronoUnit.SECONDS))
              } yield
                (earlierAsOfCorrespondingToRevision,
                  laterAsOfSharingTheSameRevisionAsTheEarlierOne,
                  revision)) filter (PartialFunction.cond(_) {
                case (earlierAsOfCorrespondingToRevision,
                laterAsOfSharingTheSameRevisionAsTheEarlierOne,
                _) =>
                  earlierAsOfCorrespondingToRevision isBefore laterAsOfSharingTheSameRevisionAsTheEarlierOne
              })

              val checks =
                for {
                  (earlierAsOfCorrespondingToRevision,
                  laterAsOfSharingTheSameRevisionAsTheEarlierOne,
                  revision) <- asOfsAndSharedRevisionTriples
                  baselineScope = world
                    .scopeFor(queryWhen, earlierAsOfCorrespondingToRevision)
                  scopeForLaterAsOfSharingTheSameRevisionAsTheEarlierOne = world
                    .scopeFor(queryWhen,
                      laterAsOfSharingTheSameRevisionAsTheEarlierOne)
                  NonExistentRecordings(historyId, historiesFrom, _) <- recordingsGroupedById flatMap (_.doesNotExistAt(
                    queryWhen))
                  if (historiesFrom(baselineScope).isEmpty) // It might not be, because we may be at an 'asOf' before the annihilation was introduced.
                } yield
                  (historyId,
                    historiesFrom,
                    baselineScope,
                    scopeForLaterAsOfSharingTheSameRevisionAsTheEarlierOne)

              if (checks.nonEmpty)
                Prop.all(checks.map {
                  case (
                    historyId,
                    historiesFrom,
                    baselineScope,
                    scopeForLaterAsOfSharingTheSameRevisionAsTheEarlierOne) =>
                    (historiesFrom(
                      scopeForLaterAsOfSharingTheSameRevisionAsTheEarlierOne).isEmpty) :| s"For ${historyId}, neither scope should yield a history."
                }: _*)
              else Prop.undecided
          }
      })
    }

    it should "reveal the same history from a scope with an 'asOf' limit that comes at or after that revision but before the following revision" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen,
          random)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        recordingsGroupedById,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        queryWhen,
        random) =>
          worldResource acquireAndGet {
            world =>
              val revisions = recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                asOfs,
                world)

              val asOfComingAfterTheLastRevision = asOfs.last.plusSeconds(10L)

              val asOfPairs = asOfs
                .scanRight((asOfComingAfterTheLastRevision,
                  asOfComingAfterTheLastRevision)) {
                  case (asOf, (laterAsOf, _)) => (asOf, laterAsOf)
                } init

              val asOfsAndSharedRevisionTriples = (for {
                ((earlierAsOfCorrespondingToRevision,
                laterAsOfComingNoLaterThanAnySucceedingRevision),
                revision) <- asOfPairs zip revisions
                laterAsOfSharingTheSameRevisionAsTheEarlierOne = earlierAsOfCorrespondingToRevision plusSeconds random
                  .chooseAnyNumberFromZeroToOneLessThan(
                    earlierAsOfCorrespondingToRevision.until(
                      laterAsOfComingNoLaterThanAnySucceedingRevision,
                      ChronoUnit.SECONDS))
              } yield
                (earlierAsOfCorrespondingToRevision,
                  laterAsOfSharingTheSameRevisionAsTheEarlierOne,
                  revision)) filter (PartialFunction.cond(_) {
                case (earlierAsOfCorrespondingToRevision,
                laterAsOfSharingTheSameRevisionAsTheEarlierOne,
                _) =>
                  earlierAsOfCorrespondingToRevision isBefore laterAsOfSharingTheSameRevisionAsTheEarlierOne
              })

              val checks =
                (for {
                  (earlierAsOfCorrespondingToRevision,
                  laterAsOfSharingTheSameRevisionAsTheEarlierOne,
                  revision) <- asOfsAndSharedRevisionTriples
                  baselineScope = world
                    .scopeFor(queryWhen, earlierAsOfCorrespondingToRevision)
                  scopeForLaterAsOfSharingTheSameRevisionAsTheEarlierOne = world
                    .scopeFor(queryWhen,
                      laterAsOfSharingTheSameRevisionAsTheEarlierOne)
                  RecordingsNoLaterThan(historyId, historiesFrom, _, _, _) <- recordingsGroupedById flatMap (_.thePartNoLaterThan(
                    queryWhen)) filter (_.historiesFrom(baselineScope).nonEmpty)
                  Seq(baselineHistory) = historiesFrom(baselineScope)
                  Seq(historyUnderTest) = historiesFrom(
                    scopeForLaterAsOfSharingTheSameRevisionAsTheEarlierOne)
                } yield
                  baselineHistory.datums
                    .zip(historyUnderTest.datums)
                    .zipWithIndex map (historyId -> _)) flatten

              if (checks.nonEmpty)
                Prop.all(checks.map {
                  case (historyId, ((actual, expected), step)) =>
                    (actual == expected) :| s"For ${historyId}, @step ${step}, ${actual} was expected to be: ${expected}"
                }: _*)
              else Prop.undecided
          }
      })
    }

    it should "reveal the same next revision from a scope with an 'asOf' limit that comes at or after that revision but before the following revision" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen,
          random)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        queryWhen,
        random) =>
          worldResource acquireAndGet { world =>
            val revisions = recordEventsInWorld(
              liftRecordings(bigShuffledHistoryOverLotsOfThings),
              asOfs,
              world)

            val asOfComingAfterTheLastRevision = asOfs.last.plusSeconds(10L)

            val asOfPairs = asOfs
              .scanRight((asOfComingAfterTheLastRevision,
                asOfComingAfterTheLastRevision)) {
                case (asOf, (laterAsOf, _)) => (asOf, laterAsOf)
              } init

            val asOfsAndSharedRevisionTriples = (for {
              ((earlierAsOfCorrespondingToRevision,
              laterAsOfComingNoLaterThanAnySucceedingRevision),
              revision) <- asOfPairs zip revisions
              laterAsOfSharingTheSameRevisionAsTheEarlierOne = earlierAsOfCorrespondingToRevision plusSeconds random
                .chooseAnyNumberFromZeroToOneLessThan(
                  earlierAsOfCorrespondingToRevision.until(
                    laterAsOfComingNoLaterThanAnySucceedingRevision,
                    ChronoUnit.SECONDS))
            } yield
              (earlierAsOfCorrespondingToRevision,
                laterAsOfSharingTheSameRevisionAsTheEarlierOne,
                revision)) filter (PartialFunction.cond(_) {
              case (earlierAsOfCorrespondingToRevision,
              laterAsOfSharingTheSameRevisionAsTheEarlierOne,
              _) =>
                earlierAsOfCorrespondingToRevision isBefore laterAsOfSharingTheSameRevisionAsTheEarlierOne
            })

            val checks = for {
              (earlierAsOfCorrespondingToRevision,
              laterAsOfSharingTheSameRevisionAsTheEarlierOne,
              revision) <- asOfsAndSharedRevisionTriples
              baselineScope = world
                .scopeFor(queryWhen, earlierAsOfCorrespondingToRevision)
              scopeForLaterAsOfSharingTheSameRevisionAsTheEarlierOne = world
                .scopeFor(queryWhen,
                  laterAsOfSharingTheSameRevisionAsTheEarlierOne)
            } yield
              (baselineScope,
                scopeForLaterAsOfSharingTheSameRevisionAsTheEarlierOne)

            if (checks.nonEmpty)
              Prop.all(checks.map {
                case (baselineScope,
                scopeForLaterAsOfSharingTheSameRevisionAsTheEarlierOne) =>
                  (baselineScope.nextRevision === scopeForLaterAsOfSharingTheSameRevisionAsTheEarlierOne.nextRevision) :| s"${baselineScope.nextRevision} === ${scopeForLaterAsOfSharingTheSameRevisionAsTheEarlierOne}.nextRevision"
              }: _*)
            else Prop.undecided
          }
      })
    }

    it should "not permit an inconsistent revision to be made" in {
      val testCaseGenerator = for {
        worldResource               <- worldResourceGenerator
        faultyRecordingsGroupedById <- faultyRecordingsGroupedByIdGenerator
        seed                        <- seedGenerator
        random = new Random(seed)
        bigShuffledFaultyHistoryOverLotsOfThings = random
          .splitIntoNonEmptyPieces(
            shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
              random,
              faultyRecordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledFaultyHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          faultyRecordingsGroupedById,
          bigShuffledFaultyHistoryOverLotsOfThings,
          asOfs,
          queryWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        faultyRecordingsGroupedById,
        bigShuffledFaultyHistoryOverLotsOfThings,
        asOfs,
        queryWhen) =>
          worldResource acquireAndGet {
            world =>
              (try {
                recordEventsInWorld(
                  liftRecordings(bigShuffledFaultyHistoryOverLotsOfThings),
                  asOfs,
                  world)
                Prop.falsified
              } catch {
                case exception if exception == WorldSpecSupport.changeError =>
                  Prop.proved
              }) :| "An exception should have been thrown when making an inconsistent revision."
          }
      })
    }

    it should "reveal all the history up to the 'when' limit of a scope made from it" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        recordingsGroupedById,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        queryWhen) =>
          worldResource acquireAndGet {
            world =>
              recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                asOfs,
                world)

              val scope = world.scopeFor(queryWhen, world.nextRevision)

              val checks = for {
                RecordingsNoLaterThan(
                historyId,
                historiesFrom,
                pertinentRecordings,
                _,
                _) <- recordingsGroupedById flatMap (_.thePartNoLaterThan(
                  queryWhen))
                Seq(history) = historiesFrom(scope)
              } yield (historyId, history.datums, pertinentRecordings.map(_._1))

              if (checks.nonEmpty)
                Prop.all(checks.map {
                  case (historyId, actualHistory, expectedHistory) =>
                    ((actualHistory.length == expectedHistory.length) :| s"For ${historyId}, the number of datums: ${actualHistory.length} was expected to be to: ${expectedHistory.length} - actual history: ${actualHistory}, expected history: ${expectedHistory}") &&
                      Prop.all(
                        (actualHistory zip expectedHistory zipWithIndex) map {
                          case ((actual, expected), step) =>
                            (actual == expected) :| s"For ${historyId}, @step ${step}, the datum: ${actual}, was expected to be: ${expected}"
                        }: _*)
                }: _*)
              else Prop.undecided
          }
      })
    }

    it should "build an item's state in a manner consistent with the history experienced by the item due to events that define changes on it." in {
      val itemId = "Fred"

      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        eventTimes    <- Gen.nonEmptyListOf(instantGenerator) map (_.sorted)
        sequence = 1 to eventTimes.size
        events: List[(Unbounded[Instant], Event)] = eventTimes zip sequence map {
          case (when, step) =>
            Finite(when) -> Change
              .forOneItem[StrictlyIncreasingSequenceConsumer](when)(itemId, {
              item: StrictlyIncreasingSequenceConsumer =>
                item.consume(step)
            })
        }
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhenForAGivenItem(
            random,
            events).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen,
          sequence)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        queryWhen,
        sequence) =>
          worldResource acquireAndGet {
            world =>
              recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                asOfs,
                world)

              val scope = world.scopeFor(queryWhen, world.nextRevision)

              val _ = scope
                .render(
                  Bitemporal.withId[StrictlyIncreasingSequenceConsumer](itemId))
                .force

              Prop.proved
          }
      })
    }

    it should "allow an item to be rendered from a bitemporal if the 'when' limit of the scope includes a relevant event that defines said item." in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        recordingsGroupedById,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        queryWhen) =>
          worldResource acquireAndGet {
            world =>
              recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                asOfs,
                world)

              val scope = world.scopeFor(queryWhen, world.nextRevision)

              val checks = for {
                RecordingsNoLaterThan(historyId, historiesFrom, _, _, _) <- recordingsGroupedById flatMap (_.thePartNoLaterThan(
                  queryWhen))
              } yield (historiesFrom, historyId)

              if (checks.nonEmpty)
                Prop.all(checks.map {
                  case (historiesFrom, historyId) => {
                    (historiesFrom(scope) match {
                      case Seq(_) => true
                    }) :| s"Could not find a history for id: ${historyId}."
                  }
                }: _*)
              else Prop.undecided
          }
      })
    }

    it should "not allow the history to be altered by ineffective events." in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator filter (_ < PositiveInfinity())
      } yield
        (worldResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        recordingsGroupedById,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        queryWhen) =>
          worldResource acquireAndGet {
            world =>
              recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                asOfs,
                world)

              for ((RecordingsNoLaterThan(
              historyId,
              _,
              _,
              ineffectiveEventFor,
              _)) <- recordingsGroupedById flatMap (_.thePartNoLaterThan(
                queryWhen))) {
                world.revise(Map(-1 -> Some(ineffectiveEventFor(queryWhen))),
                  asOfs.last)
              }

              val scope = world.scopeFor(queryWhen, world.nextRevision)

              val checks = for {
                RecordingsNoLaterThan(historyId, historiesFrom, _, _, _) <- recordingsGroupedById flatMap (_.thePartNoLaterThan(
                  queryWhen))
              } yield (historiesFrom, historyId)

              if (checks.nonEmpty)
                Prop.all(checks.map {
                  case (historiesFrom, historyId) => {
                    (historiesFrom(scope) match {
                      case Seq(_) => true
                    }) :| s"Could not find a history for id: ${historyId}."
                  }
                }: _*)
              else Prop.undecided
          }
      })
    }

    it should "reveal all the history of a related item up to the 'when' limit of a scope made from it" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        referencedHistoryRecordingsGroupedById <- referencedHistoryRecordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        referringHistoryRecordingsGroupedById <- referringHistoryRecordingsGroupedByIdGenerator(
          forbidMeasurements = true)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            referencedHistoryRecordingsGroupedById ++ referringHistoryRecordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          referencedHistoryRecordingsGroupedById,
          referringHistoryRecordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen)
      check(
        Prop.forAllNoShrink(testCaseGenerator) {
          case (worldResource,
          referencedHistoryRecordingsGroupedById,
          referringHistoryRecordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen) =>
            worldResource acquireAndGet { world =>
              recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                asOfs,
                world)

              val scope = world.scopeFor(queryWhen, world.nextRevision)

              val checks = for {
                RecordingsNoLaterThan(
                referringHistoryId,
                referringHistoriesFrom,
                _,
                _,
                _) <- referringHistoryRecordingsGroupedById flatMap (_.thePartNoLaterThan(
                  queryWhen))
                RecordingsNoLaterThan(
                referencedHistoryId,
                _,
                pertinentRecordings,
                _,
                _) <- referencedHistoryRecordingsGroupedById flatMap (_.thePartNoLaterThan(
                  queryWhen))
                Seq(referringHistory: ReferringHistory) = referringHistoriesFrom(
                  scope) if referringHistory.referencedDatums.contains(
                  referencedHistoryId) && !referringHistory
                  .referencedHistories(referencedHistoryId)
                  .asInstanceOf[ItemExtensionApi]
                  .isGhost
              } yield
                (referringHistoryId,
                  referencedHistoryId,
                  referringHistory.referencedDatums(referencedHistoryId),
                  pertinentRecordings.map(_._1))

              if (checks.nonEmpty)
                Prop.all(checks.map {
                  case (referringHistoryId,
                  referencedHistoryId,
                  actualHistory,
                  expectedHistory) =>
                    ((actualHistory.length == expectedHistory.length) :| s"For referring history id: ${referringHistoryId}, referenced history id: ${referencedHistoryId}, the number of datums: ${actualHistory.length} was expected to be to: ${expectedHistory.length}") &&
                      Prop.all(
                        (actualHistory zip expectedHistory zipWithIndex) map {
                          case ((actual, expected), step) =>
                            (actual == expected) :| s"For referring history id: ${referringHistoryId}, referenced history id: ${referencedHistoryId}, @step ${step}, the datum: ${actual}, was expected to be: ${expected}"
                        }: _*)
                }: _*)
              else Prop.undecided
            }
        },
        minSuccessful(12),
        maxSize(5)
      )
    }

    it should "yield the same identity for a related item as when that item is directly queried for" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        referencedHistoryRecordingsGroupedById <- referencedHistoryRecordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        referringHistoryRecordingsGroupedById <- referringHistoryRecordingsGroupedByIdGenerator(
          forbidMeasurements = true)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            referencedHistoryRecordingsGroupedById ++ referringHistoryRecordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          referencedHistoryRecordingsGroupedById,
          referringHistoryRecordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen)
      check(
        Prop.forAllNoShrink(testCaseGenerator) {
          case (worldResource,
          referencedHistoryRecordingsGroupedById,
          referringHistoryRecordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen) =>
            worldResource acquireAndGet { world =>
              recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                asOfs,
                world)

              val scope = world.scopeFor(queryWhen, world.nextRevision)

              val checks = for {
                RecordingsNoLaterThan(
                referringHistoryId,
                referringHistoriesFrom,
                _,
                _,
                _) <- referringHistoryRecordingsGroupedById flatMap (_.thePartNoLaterThan(
                  queryWhen))
                RecordingsNoLaterThan(
                referencedHistoryId,
                referencedHistoriesFrom,
                pertinentRecordings,
                _,
                _) <- referencedHistoryRecordingsGroupedById flatMap (_.thePartNoLaterThan(
                  queryWhen))
                Seq(referringHistory: ReferringHistory) = referringHistoriesFrom(
                  scope) if referringHistory.referencedDatums.contains(
                  referencedHistoryId) && !referringHistory
                  .referencedHistories(referencedHistoryId)
                  .asInstanceOf[ItemExtensionApi]
                  .isGhost
              } yield (referringHistoryId, referencedHistoryId)

              if (checks.nonEmpty)
                Prop.all(checks.map {
                  case (referringHistoryId, referencedHistoryId) =>
                    val directAccessBitemporalQuery: Bitemporal[History] =
                      Bitemporal.withId[History](
                        referencedHistoryId.asInstanceOf[History#Id])
                    val indirectAccessBitemporalQuery
                    : Bitemporal[History] = Bitemporal
                      .withId[ReferringHistory](referringHistoryId.asInstanceOf[
                      ReferringHistory#Id]) map (_.referencedHistories(
                      referencedHistoryId))
                    val agglomeratedBitemporalQuery
                    : Bitemporal[(History, History)] =
                      (directAccessBitemporalQuery |@| indirectAccessBitemporalQuery)(
                        (_: History, _: History))
                    val Seq((directlyAccessedReferencedHistory: History,
                    indirectlyAccessedReferencedHistory: History)) =
                      scope.render(agglomeratedBitemporalQuery)
                    (directlyAccessedReferencedHistory eq indirectlyAccessedReferencedHistory) :| s"Expected item: '$indirectlyAccessedReferencedHistory' accessed indirectly via referring item of id: '$referringHistoryId' to have the same object identity as '$directlyAccessedReferencedHistory' accessed directly via id: '$referencedHistoryId'."
                }: _*)
              else Prop.undecided
            }
        },
        minSuccessful(12),
        maxSize(5)
      )
    }

    it should "not reveal an item at a query time coming before its first defining event" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        recordingsGroupedById,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        queryWhen) =>
          worldResource acquireAndGet {
            world =>
              recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                asOfs,
                world)

              val scope = world.scopeFor(queryWhen, world.nextRevision)

              val checks = for {
                NonExistentRecordings(historyId, historiesFrom, _) <- recordingsGroupedById flatMap (_.doesNotExistAt(
                  queryWhen))
                histories = historiesFrom(scope)
              } yield (historyId, histories)

              if (checks.nonEmpty)
                Prop.all(checks.map {
                  case (historyId, histories) =>
                    histories.isEmpty :| s"For ${historyId}, ${histories}.isEmpty"
                }: _*)
              else Prop.undecided
          }
      })
    }

    it should "not consider an ineffective event as being defining" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen)
      check(
        Prop
          .forAllNoShrink(testCaseGenerator) {
            case (worldResource,
            recordingsGroupedById,
            bigShuffledHistoryOverLotsOfThings,
            asOfs,
            queryWhen) =>
              worldResource acquireAndGet {
                world =>
                  recordEventsInWorld(
                    liftRecordings(bigShuffledHistoryOverLotsOfThings),
                    asOfs,
                    world)

                  for ((NonExistentRecordings(
                  historyId,
                  _,
                  ineffectiveEventFor)) <- recordingsGroupedById flatMap (_.doesNotExistAt(
                    queryWhen))) {
                    world.revise(
                      Map(-1 -> Some(ineffectiveEventFor(NegativeInfinity()))),
                      asOfs.last)
                    if (queryWhen < PositiveInfinity()) {
                      world.revise(
                        Map(-2 -> Some(ineffectiveEventFor(queryWhen))),
                        asOfs.last)
                    }
                  }

                  val scope = world.scopeFor(queryWhen, world.nextRevision)

                  val checks = for {
                    NonExistentRecordings(historyId, historiesFrom, _) <- recordingsGroupedById flatMap (_.doesNotExistAt(
                      queryWhen))
                    histories = historiesFrom(scope)
                  } yield (historyId, histories)

                  if (checks.nonEmpty)
                    Prop.all(checks.map {
                      case (historyId, histories) =>
                        histories.isEmpty :| s"For ${historyId}, ${histories}.isEmpty"
                    }: _*)
                  else Prop.undecided
              }
          })
    }

    it should "consider a reference to a related item in an event as being defining" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        referencedHistoryRecordingsGroupedById <- referencedHistoryRecordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        referringHistoryRecordingsGroupedById <- referringHistoryRecordingsGroupedByIdGenerator(
          forbidMeasurements = true)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            referencedHistoryRecordingsGroupedById ++ referringHistoryRecordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          referencedHistoryRecordingsGroupedById,
          referringHistoryRecordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen)
      check(
        Prop.forAllNoShrink(testCaseGenerator) {
          case (worldResource,
          referencedHistoryRecordingsGroupedById,
          referringHistoryRecordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen) =>
            worldResource acquireAndGet {
              world =>
                recordEventsInWorld(
                  liftRecordings(bigShuffledHistoryOverLotsOfThings),
                  asOfs,
                  world)

                val scope = world.scopeFor(queryWhen, world.nextRevision)

                val checks: List[(Any, History)] =
                  for {
                    RecordingsNoLaterThan(
                    referringHistoryId,
                    referringHistoriesFrom,
                    _,
                    _,
                    _) <- referringHistoryRecordingsGroupedById flatMap (_.thePartNoLaterThan(
                      queryWhen))
                    NonExistentRecordings(
                    referencedHistoryId,
                    referencedHistoriesFrom,
                    _) <- referencedHistoryRecordingsGroupedById flatMap (_.doesNotExistAt(
                      queryWhen))
                    Seq(referringHistory: ReferringHistory) = referringHistoriesFrom(
                      scope) if referringHistory.referencedDatums.contains(
                      referencedHistoryId) && !referringHistory
                      .referencedHistories(referencedHistoryId)
                      .asInstanceOf[ItemExtensionApi]
                      .isGhost
                    Seq(referencedHistory) = referencedHistoriesFrom(scope)
                  } yield (referencedHistoryId, referencedHistory)

                if (checks.nonEmpty)
                  Prop.all(checks.map {
                    case (historyId, actualHistory) =>
                      (actualHistory.datums.isEmpty :| s"For ${historyId}, the datums: ${actualHistory.datums} was supposed to be empty")
                  }: _*)
                else Prop.undecided
            }
        },
        minSuccessful(12),
        maxSize(5)
      )
    }

    it should "detect the application of measurements that would attempt to define a future item whose existence would overlap with and conflict with an existing item that is subsequently annihilated." in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        referencedHistoryRecordingsGroupedById <- referencedHistoryRecordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            referencedHistoryRecordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        referencingEventWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          referencedHistoryRecordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          referencingEventWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        referencedHistoryRecordingsGroupedById,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        referencingEventWhen) =>
          worldResource acquireAndGet {
            world =>
              recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                asOfs,
                world)

              val checks = for {
                RecordingsNoLaterThan(
                referencedHistoryId: History#Id,
                _,
                _,
                _,
                whenAnnihilated) <- referencedHistoryRecordingsGroupedById flatMap (_.thePartNoLaterThan(
                  referencingEventWhen))
                whenTheReferencedItemIsAnnihilated <- whenAnnihilated.toList
              } yield (referencedHistoryId, whenTheReferencedItemIsAnnihilated)

              val theReferrerIdBase = "The Referrer"

              val unimportantReferencedHistoryId = "Groucho"

              if (checks.nonEmpty) {
                for (((referencedHistoryId, whenTheReferencedItemIsAnnihilated),
                index) <- checks zipWithIndex) {
                  val theReferrerId = s"$theReferrerIdBase - $index"

                  val change = Change.forTwoItems[ReferringHistory, History](
                    referencingEventWhen)(
                    theReferrerId,
                    unimportantReferencedHistoryId.asInstanceOf[History#Id],
                    (referringHistory: ReferringHistory,
                     referencedItem: History) => {
                      referringHistory.referTo(referencedItem)
                    }
                  )

                  val measurement = Measurement
                    .forTwoItems[ReferringHistory, MoreSpecificFooHistory](
                    whenTheReferencedItemIsAnnihilated)(
                    theReferrerId,
                    referencedHistoryId
                      .asInstanceOf[MoreSpecificFooHistory#Id],
                    (referringHistory: ReferringHistory,
                     referencedItem: MoreSpecificFooHistory) => {
                      referringHistory.referTo(referencedItem)
                    }
                  )

                  intercept[RuntimeException] {
                    world.revise(Map(-1 - (2 * index)   -> Some(change),
                      -(2 * (index + 1)) -> Some(measurement)),
                      world.revisionAsOfs.last)
                  }
                }

                Prop.proved
              } else Prop.undecided
          }
      })
    }

    it should "detect the application of measurements that would attempt to define a future item whose existence would overlap with and conflict with an existing item that is subsequently annihilated - with a twist." in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        referencedHistoryRecordingsGroupedById <- referencedHistoryRecordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            referencedHistoryRecordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        probeWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          referencedHistoryRecordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          probeWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        referencedHistoryRecordingsGroupedById,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        probeWhen) =>
          worldResource acquireAndGet {
            world =>
              val checks = for {
                RecordingsNoLaterThan(
                referencedHistoryId: History#Id,
                _,
                _,
                _,
                whenAnnihilated) <- referencedHistoryRecordingsGroupedById flatMap (_.thePartNoLaterThan(
                  probeWhen))
                whenTheReferencedItemIsAnnihilated <- whenAnnihilated.toList
              } yield (referencedHistoryId, whenTheReferencedItemIsAnnihilated)

              val theReferrerIdBase = "The Referrer"

              val unimportantReferencedHistoryId = "Groucho"

              if (checks.nonEmpty) {
                for (((referencedHistoryId, whenTheReferencedItemIsAnnihilated),
                index) <- checks zipWithIndex) {
                  val theReferrerId = s"$theReferrerIdBase - $index"

                  val change =
                    Change.forTwoItems[ReferringHistory, FooHistory](
                      whenTheReferencedItemIsAnnihilated)(
                      theReferrerId,
                      unimportantReferencedHistoryId
                        .asInstanceOf[FooHistory#Id],
                      (referringHistory: ReferringHistory,
                       referencedItem: FooHistory) => {
                        referringHistory.referTo(referencedItem)
                      }
                    )

                  world.revise(Map(-1 - (2 * index) -> Some(change)),
                    asOfs.head)
                }

                recordEventsInWorld(
                  liftRecordings(bigShuffledHistoryOverLotsOfThings),
                  asOfs,
                  world)

                for (((referencedHistoryId, whenTheReferencedItemIsAnnihilated),
                index) <- checks zipWithIndex) {
                  val theReferrerId = s"$theReferrerIdBase - $index"

                  val measurement = Measurement
                    .forTwoItems[ReferringHistory, MoreSpecificFooHistory](
                    whenTheReferencedItemIsAnnihilated)(
                    theReferrerId,
                    referencedHistoryId
                      .asInstanceOf[MoreSpecificFooHistory#Id],
                    (referringHistory: ReferringHistory,
                     referencedItem: MoreSpecificFooHistory) => {
                      referringHistory.referTo(referencedItem)
                    }
                  )

                  intercept[RuntimeException] {
                    world.revise(Map(-(2 * (index + 1)) -> Some(measurement)),
                      asOfs.head)
                  }
                }

                Prop.proved
              } else Prop.undecided
          }
      })
    }

    it should "treat an annihilated item accessed via a reference to a related item as being a ghost" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        referencedHistoryRecordingsGroupedById <- referencedHistoryRecordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            referencedHistoryRecordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        referencingEventWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          referencedHistoryRecordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          referencingEventWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        referencedHistoryRecordingsGroupedById,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        referencingEventWhen) =>
          worldResource acquireAndGet { world =>
            recordEventsInWorld(
              liftRecordings(bigShuffledHistoryOverLotsOfThings),
              asOfs,
              world)

            val checks = for {
              RecordingsNoLaterThan(
              referencedHistoryId: History#Id,
              _,
              _,
              _,
              whenAnnihilated) <- referencedHistoryRecordingsGroupedById flatMap (_.thePartNoLaterThan(
                referencingEventWhen))

              // Have to make sure the referenced item is annihilated *after* the event making the reference to it,
              // otherwise that event will have caused the creation of a new lifecycle for the referenced item instead.
              whenAnnihilated <- whenAnnihilated.toList
              if whenAnnihilated > referencingEventWhen
            } yield (referencedHistoryId, whenAnnihilated)

            val theReferrerId = "The Referrer"

            for (((referencedHistoryId, _), index) <- checks zipWithIndex) {
              world.revise(
                Map(
                  -1 - index -> Some(
                    Change.forTwoItems[ReferringHistory, History](
                      referencingEventWhen)(theReferrerId,
                      referencedHistoryId,
                      (referringHistory: ReferringHistory,
                       referencedItem: History) => {
                        referringHistory.referTo(
                          referencedItem)
                      }))),
                world.revisionAsOfs.last
              )
            }

            if (checks.nonEmpty)
              Prop.all(checks.map {
                case (referencedHistoryId, laterQueryWhenAtAnnihilation) => {
                  val scope = world.scopeFor(laterQueryWhenAtAnnihilation,
                    world.nextRevision)
                  val Seq(referringHistory) = scope.render(
                    Bitemporal.withId[ReferringHistory](theReferrerId))
                  val ghostItem =
                    referringHistory.referencedHistories(referencedHistoryId)
                  val idOfGhost = ghostItem.id // It's OK to ask a ghost what its name is.
                  val itIsAGhost = ghostItem
                    .asInstanceOf[ItemExtensionApi]
                    .isGhost // It's OK to ask a ghost to prove its ghostliness.
                  intercept[RuntimeException] {
                    ghostItem.datums // It's not OK to ask any other questions - it will just go 'Whooh' at you.
                  }
                  (idOfGhost == referencedHistoryId) :| s"Expected referenced item of id: '$referencedHistoryId' referred to by item of id: '${referringHistory.id}' to reveal its id correctly - but got '$idOfGhost' instead."
                  itIsAGhost :| s"Expected referenced item of id: '$referencedHistoryId' referred to by item of id: '${referringHistory.id}' to be a ghost at time: $laterQueryWhenAtAnnihilation - the event causing referral was at: $referencingEventWhen."
                }
              }: _*)
            else Prop.undecided
          }
      })
    }

    it should "not allow an event to either refer to or to mutate the state of a related item that is a ghost" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        referencedHistoryRecordingsGroupedById <- referencedHistoryRecordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            referencedHistoryRecordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        referencingEventWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          referencedHistoryRecordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          referencingEventWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        referencedHistoryRecordingsGroupedById,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        referencingEventWhen) =>
          worldResource acquireAndGet { world =>
            recordEventsInWorld(
              liftRecordings(bigShuffledHistoryOverLotsOfThings),
              asOfs,
              world)

            val checks = for {
              RecordingsNoLaterThan(
              referencedHistoryId: History#Id,
              _,
              _,
              _,
              whenAnnihilated) <- referencedHistoryRecordingsGroupedById flatMap (_.thePartNoLaterThan(
                referencingEventWhen))

              // Have to make sure the referenced item is annihilated *after* the event making the reference to it,
              // otherwise that event will have caused the creation of a new lifecycle for the referenced item instead.
              whenAnnihilated <- whenAnnihilated.toList
              if whenAnnihilated > referencingEventWhen
            } yield (referencedHistoryId, whenAnnihilated)

            val theReferrerId = "The Referrer"

            for (((referencedHistoryId, _), index) <- checks zipWithIndex) {
              world.revise(
                Map(
                  -1 - index -> Some(
                    Change.forTwoItems[ReferringHistory, History](
                      referencingEventWhen)(theReferrerId,
                      referencedHistoryId,
                      (referringHistory: ReferringHistory,
                       referencedItem: History) => {
                        referringHistory.referTo(
                          referencedItem)
                      }))),
                world.revisionAsOfs.last
              )
            }

            if (checks.nonEmpty)
              Prop.all(checks.zipWithIndex.map {
                case ((referencedHistoryId, whenAnnihilated), index) => {
                  try {
                    intercept[RuntimeException] {
                      world.revise(
                        Map(-2 - index -> Some(
                          Change.forOneItem[ReferringHistory](whenAnnihilated)(
                            theReferrerId,
                            (referringHistory: ReferringHistory) => {
                              referringHistory.mutateRelatedItem(
                                referencedHistoryId)
                            }))),
                        world.revisionAsOfs.last
                      )
                    }
                  } catch {
                    case exception: TestFailedException =>
                      println(
                        s"Failed to detect mutation of a ghost, referring item id: $theReferrerId, referenced item id: $referencedHistoryId, when: $whenAnnihilated, when the relationship was set up: $referencingEventWhen.")
                      throw exception
                  }

                  try {
                    intercept[RuntimeException] {
                      world.revise(
                        Map(-3 - index -> Some(
                          Change.forOneItem[ReferringHistory](whenAnnihilated)(
                            theReferrerId,
                            (referringHistory: ReferringHistory) => {
                              referringHistory.referToRelatedItem(
                                referencedHistoryId)
                            }))),
                        world.revisionAsOfs.last
                      )
                    }
                  } catch {
                    case exception: TestFailedException =>
                      println(
                        s"Failed to detect mutation of a ghost, referring item id: $theReferrerId, referenced item id: $referencedHistoryId, when: $whenAnnihilated, when the relationship was set up: $referencingEventWhen.")
                      throw exception
                  }

                  Prop.proved
                }
              }: _*)
            else Prop.undecided
          }
      })
    }

    it should "not permit the annihilation of an item at a query time coming before its first defining event" in {
      val testCaseGenerator = for {
        worldResource         <- worldResourceGenerator
        recordingsGroupedById <- integerHistoryRecordingsGroupedByIdGenerator
        seed                  <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        definiteQueryWhen <- instantGenerator
      } yield
        (worldResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          definiteQueryWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        recordingsGroupedById,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        definiteQueryWhen) =>
          worldResource acquireAndGet {
            world =>
              recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                asOfs,
                world)

              val scope =
                world.scopeFor(Finite(definiteQueryWhen), world.nextRevision)

              Prop.all((for {
                NonExistentRecordings(historyId, historiesFrom, _) <- recordingsGroupedById flatMap (_.doesNotExistAt(
                  Finite(definiteQueryWhen)))
                histories = historiesFrom(scope)
              } yield {
                intercept[RuntimeException] {
                  val eventIdForAnnihilation = -1
                  world.revise(eventIdForAnnihilation,
                    Annihilation[IntegerHistory](
                      definiteQueryWhen,
                      historyId.asInstanceOf[String]),
                    asOfs.last)
                }
                Prop.proved
              } :| s"Should have rejected the attempt to annihilate an item that didn't exist at the query time."): _*)
          }
      })
    }

    it should "have a next revision that reflects the last added revision" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
      } yield (worldResource, bigShuffledHistoryOverLotsOfThings, asOfs)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource, bigShuffledHistoryOverLotsOfThings, asOfs) =>
          worldResource acquireAndGet { world =>
            val revisions = recordEventsInWorld(
              liftRecordings(bigShuffledHistoryOverLotsOfThings),
              asOfs,
              world)

            (1 + revisions.last === world.nextRevision) :| s"1 + ${revisions}.last === ${world.nextRevision}"
          }
      })
    }

    it should "have a version timeline that records the 'asOf' time for each of its revisions" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
      } yield (worldResource, bigShuffledHistoryOverLotsOfThings, asOfs)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource, bigShuffledHistoryOverLotsOfThings, asOfs) =>
          worldResource acquireAndGet { world =>
            recordEventsInWorld(
              liftRecordings(bigShuffledHistoryOverLotsOfThings),
              asOfs,
              world)

            Prop.all(asOfs zip world.revisionAsOfs map {
              case (asOf, timelineAsOf) =>
                (asOf === timelineAsOf) :| s"${asOf} === ${timelineAsOf}"
            }: _*)
          }
      })
    }

    it should "have a sorted version timeline" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
      } yield (worldResource, bigShuffledHistoryOverLotsOfThings, asOfs)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource, bigShuffledHistoryOverLotsOfThings, asOfs) =>
          worldResource acquireAndGet { world =>
            recordEventsInWorld(
              liftRecordings(bigShuffledHistoryOverLotsOfThings),
              asOfs,
              world)

            Prop.all(world.revisionAsOfs zip world.revisionAsOfs.tail map {
              case (first, second) =>
                !first.isAfter(second) :| s"!${first}.isAfter(${second})"
            }: _*)
          }
      })
    }

    it should "allocate revision numbers sequentially" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
      } yield (worldResource, bigShuffledHistoryOverLotsOfThings, asOfs)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource, bigShuffledHistoryOverLotsOfThings, asOfs) =>
          worldResource acquireAndGet { world =>
            val revisions = recordEventsInWorld(
              liftRecordings(bigShuffledHistoryOverLotsOfThings),
              asOfs,
              world)

            Prop.all((revisions zipWithIndex) map {
              case (revision, index) =>
                (index === revision) :| s"${index} === ${revision}"
            }: _*)
          }
      })
    }

    it should "have a next revision number that is the size of its version timeline" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
      } yield (worldResource, bigShuffledHistoryOverLotsOfThings, asOfs)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource, bigShuffledHistoryOverLotsOfThings, asOfs) =>
          worldResource acquireAndGet { world =>
            recordEventsInWorld(
              liftRecordings(bigShuffledHistoryOverLotsOfThings),
              asOfs,
              world)

            (world.nextRevision === world.revisionAsOfs.length) :| s"${world.nextRevision} === ${world.revisionAsOfs}.length"
          }
      })
    }

    it should "not permit the 'asOf' time for a new revision to be less than that of any existing revision." in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(
          bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted) filter (1 < _.toSet.size) // Make sure we have at least two revisions at different times.
      } yield (worldResource, bigShuffledHistoryOverLotsOfThings, asOfs, random)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        random) =>
          worldResource acquireAndGet {
            world =>
              val numberOfRevisions = asOfs.length

              val candidateIndicesToStartATranspose =
                asOfs.zip(asOfs.tail).zipWithIndex filter {
                  case ((first, second), index) => first isBefore second
                } map (_._2) toSeq

              val indexOfFirstAsOfBeingTransposed =
                random.chooseOneOf(candidateIndicesToStartATranspose)

              val asOfsWithIncorrectTransposition =
                asOfs.splitAt(indexOfFirstAsOfBeingTransposed) match {
                  case (asOfsBeforeTransposition,
                  Seq(first, second, asOfsAfterTransposition @ _*)) =>
                    asOfsBeforeTransposition ++ Seq(second, first) ++ asOfsAfterTransposition
                }

              {
                intercept[IllegalArgumentException](
                  recordEventsInWorld(
                    liftRecordings(bigShuffledHistoryOverLotsOfThings),
                    asOfsWithIncorrectTransposition,
                    world))
                Prop.proved
              } :| s"Using ${asOfsWithIncorrectTransposition} should cause a precondition failure."
          }
      })
    }

    it should "create a scope whose properties relate to the call to 'scopeFor' when using the next revision overload" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource, bigShuffledHistoryOverLotsOfThings, asOfs, queryWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        queryWhen) =>
          worldResource acquireAndGet {
            world =>
              recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                asOfs,
                world)

              val expectedAsOfBeforeInitialRevision: Unbounded[Instant] =
                NegativeInfinity[Instant]

              def asOfAndNextRevisionPairs_(
                                             nextRevisions: List[(Unbounded[Instant], (Int, Int))],
                                             asOf: Unbounded[Instant]) = nextRevisions match {
                case (preceedingAsOf,
                (preceedingNextRevision,
                preceedingNextRevisionAfterFirstDuplicate)) :: tail
                  if asOf == preceedingAsOf =>
                  (asOf -> ((1 + preceedingNextRevision) -> preceedingNextRevisionAfterFirstDuplicate)) :: tail
                case (_, (preceedingNextRevision, _)) :: _ => {
                  var nextRevision = 1 + preceedingNextRevision
                  (asOf -> (nextRevision -> nextRevision)) :: nextRevisions
                }
              }

              val asOfAndNextRevisionPairs = (List(
                expectedAsOfBeforeInitialRevision -> (World.initialRevision -> World.initialRevision)) /: asOfs
                .map(Finite(_)))(asOfAndNextRevisionPairs_) reverse

              val checksViaNextRevision = for {
                (
                  asOf,
                  (nextRevisionAfterDuplicates,
                  nextRevisionAfterFirstDuplicate)) <- asOfAndNextRevisionPairs
                nextRevision                         <- nextRevisionAfterFirstDuplicate to nextRevisionAfterDuplicates
                scopeViaNextRevision = world.scopeFor(queryWhen, nextRevision)
              } yield (asOf, nextRevision, scopeViaNextRevision)

              if (checksViaNextRevision.nonEmpty)
                Prop.all(checksViaNextRevision map {
                  case (asOf, nextRevision, scopeViaNextRevision) =>
                    (asOf === scopeViaNextRevision.asOf) :| s"${asOf} === scopeViaNextRevision.asOf" &&
                      (nextRevision === scopeViaNextRevision.nextRevision) :| s"${nextRevision} === scopeViaNextRevision.nextRevision" &&
                      (queryWhen === scopeViaNextRevision.when) :| s"${queryWhen} === scopeViaNextRevision.when"
                }: _*)
              else Prop.undecided
          }

        // TODO - perturb the 'asOf' values so as not to go the next revision and see how that plays with the two ways of constructing a scope.
      })
    }

    it should "create a scope whose properties relate to the call to 'scopeFor' when using the 'asOf' time overload" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen,
          random)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        queryWhen,
        random) =>
          worldResource acquireAndGet { world =>
            val revisions = recordEventsInWorld(
              liftRecordings(bigShuffledHistoryOverLotsOfThings),
              asOfs,
              world)

            val asOfComingAfterTheLastRevision = asOfs.last.plusSeconds(10L)

            val asOfPairs = asOfs
              .scanRight((asOfComingAfterTheLastRevision,
                asOfComingAfterTheLastRevision)) {
                case (asOf, (laterAsOf, _)) => (asOf, laterAsOf)
              } init

            val asOfsAndSharedRevisionTriples = (for {
              ((earlierAsOfCorrespondingToRevision,
              laterAsOfComingNoLaterThanAnySucceedingRevision),
              revision) <- asOfPairs zip revisions
              laterAsOfSharingTheSameRevisionAsTheEarlierOne = earlierAsOfCorrespondingToRevision plusSeconds random
                .chooseAnyNumberFromZeroToOneLessThan(
                  earlierAsOfCorrespondingToRevision.until(
                    laterAsOfComingNoLaterThanAnySucceedingRevision,
                    ChronoUnit.SECONDS))
            } yield
              (earlierAsOfCorrespondingToRevision,
                laterAsOfSharingTheSameRevisionAsTheEarlierOne,
                revision)) filter (PartialFunction.cond(_) {
              case (earlierAsOfCorrespondingToRevision,
              laterAsOfSharingTheSameRevisionAsTheEarlierOne,
              _) =>
                earlierAsOfCorrespondingToRevision isBefore laterAsOfSharingTheSameRevisionAsTheEarlierOne
            })

            val checksViaAsOf = for {
              (earlierAsOfCorrespondingToRevision,
              laterAsOfSharingTheSameRevisionAsTheEarlierOne,
              revision) <- asOfsAndSharedRevisionTriples
              scopeViaEarlierAsOfCorrespondingToRevision = world
                .scopeFor(queryWhen, earlierAsOfCorrespondingToRevision)
              scopeViaLaterAsOfSharingTheSameRevisionAsTheEarlierOne = world
                .scopeFor(queryWhen,
                  laterAsOfSharingTheSameRevisionAsTheEarlierOne)
              nextRevision = 1 + revision
            } yield
              (earlierAsOfCorrespondingToRevision,
                laterAsOfSharingTheSameRevisionAsTheEarlierOne,
                nextRevision,
                scopeViaEarlierAsOfCorrespondingToRevision,
                scopeViaLaterAsOfSharingTheSameRevisionAsTheEarlierOne)

            if (checksViaAsOf.nonEmpty)
              Prop.all(checksViaAsOf map {
                case (earlierAsOfCorrespondingToRevision,
                laterAsOfSharingTheSameRevisionAsTheEarlierOne,
                nextRevision,
                scopeViaEarlierAsOfCorrespondingToRevision,
                scopeViaLaterAsOfSharingTheSameRevisionAsTheEarlierOne) =>
                  (Finite(earlierAsOfCorrespondingToRevision) === scopeViaEarlierAsOfCorrespondingToRevision.asOf) :| s"Finite(${earlierAsOfCorrespondingToRevision}) === scopeViaEarlierAsOfCorrespondingToRevision.asOf" &&
                    (Finite(laterAsOfSharingTheSameRevisionAsTheEarlierOne) === scopeViaLaterAsOfSharingTheSameRevisionAsTheEarlierOne.asOf) :| s"Finite(${laterAsOfSharingTheSameRevisionAsTheEarlierOne}) === scopeViaLaterAsOfSharingTheSameRevisionAsTheEarlierOne.asOf" &&
                    (nextRevision === scopeViaEarlierAsOfCorrespondingToRevision.nextRevision) :| s"${nextRevision} === scopeViaEarlierAsOfCorrespondingToRevision.nextRevision" &&
                    (nextRevision === scopeViaLaterAsOfSharingTheSameRevisionAsTheEarlierOne.nextRevision) :| s"${nextRevision} === scopeViaLaterAsOfSharingTheSameRevisionAsTheEarlierOne.nextRevision" &&
                    (queryWhen == scopeViaEarlierAsOfCorrespondingToRevision.when) :| s"${queryWhen} == scopeViaEarlierAsOfCorrespondingToRevision.when" &&
                    (queryWhen == scopeViaLaterAsOfSharingTheSameRevisionAsTheEarlierOne.when) :| s"${queryWhen} == scopeViaLaterAsOfSharingTheSameRevisionAsTheEarlierOne.when"
              }: _*)
            else Prop.undecided
          }
      })
    }

    it should "create a scope that is a snapshot unaffected by subsequent revisions" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        recordingsGroupedById,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        queryWhen) =>
          worldResource acquireAndGet {
            world =>
              // What's being tested is the imperative behaviour of 'World' wrt its scopes - so use imperative code.
              val scopeViaRevisionToHistoryMap =
                scala.collection.mutable.Map.empty[Scope, List[(Any, Any)]]
              val scopeViaAsOfToHistoryMap =
                scala.collection.mutable.Map.empty[Scope, List[(Any, Any)]]

              val results = scala.collection.mutable.MutableList.empty[Prop]

              for (revisionAction <- revisionActions(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                asOfs,
                world)) {
                val revision = revisionAction()

                results += Prop.all(scopeViaRevisionToHistoryMap map {
                  case (scope, history) =>
                    (history === historyFrom(world, recordingsGroupedById)(
                      scope)) :| s"history === historyFrom(scope)"
                } toSeq: _*)
                results += Prop.all(scopeViaAsOfToHistoryMap map {
                  case (scope, history) =>
                    (history === historyFrom(world, recordingsGroupedById)(
                      scope)) :| s"history === historyFrom(scope)"
                } toSeq: _*)

                val scopeViaRevision = world.scopeFor(queryWhen, revision)
                scopeViaRevisionToHistoryMap += (scopeViaRevision -> historyFrom(
                  world,
                  recordingsGroupedById)(scopeViaRevision))
                val scopeViaAsOf =
                  world.scopeFor(queryWhen, world.revisionAsOfs(revision))
                scopeViaAsOfToHistoryMap += (scopeViaAsOf -> historyFrom(
                  world,
                  recordingsGroupedById)(scopeViaAsOf))
              }

              if (results.nonEmpty)
                Prop.all(results: _*)
              else Prop.undecided
          }
      })
    }

    it should "create revisions with the strong exception-safety guarantee" in {
      val testCaseGenerator = for {
        utopiaResource        <- worldResourceGenerator
        distopiaResource      <- worldResourceGenerator
        recordingsGroupedById <- nonConflictingRecordingsGroupedByIdGenerator // Use this flavour to avoid raising unanticipated exceptions due to interspersing
        // events referring to 'FooHistory' and 'MoreSpecificFooHistory' on the same id.
        faultyRecordingsGroupedById <- faultyRecordingsGroupedByIdGenerator
        seed                        <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random
          .splitIntoNonEmptyPieces(
            shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
              random,
              recordingsGroupedById).zipWithIndex)
          .force
        bigShuffledFaultyHistoryOverLotsOfThings = random
          .splitIntoNonEmptyPieces(
            shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
              random,
              faultyRecordingsGroupedById).zipWithIndex map {
              case (stuff, index) => stuff -> (-1 - index)
            })
          .force // Map with event ids over to strictly negative values to avoid collisions with the changes that are expected to work.
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        faultyAsOfs <- Gen.listOfN(
          bigShuffledFaultyHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (utopiaResource,
          distopiaResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          bigShuffledFaultyHistoryOverLotsOfThings,
          asOfs,
          faultyAsOfs,
          queryWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (utopiaResource,
        distopiaResource,
        recordingsGroupedById,
        bigShuffledHistoryOverLotsOfThings,
        bigShuffledFaultyHistoryOverLotsOfThings,
        asOfs,
        faultyAsOfs,
        queryWhen) =>
          utopiaResource and distopiaResource acquireAndGet {
            case (utopia, distopia) =>
              // NOTE: we add some 'good' changes within the faulty revisions to make things more realistic prior to merging the faulty history with the good history...
              val (mergedShuffledHistoryOverLotsOfThings, mergedAsOfs) =
                ((bigShuffledHistoryOverLotsOfThings zip asOfs) ++ (bigShuffledFaultyHistoryOverLotsOfThings zip bigShuffledHistoryOverLotsOfThings map {
                  case (faulty, ok) => faulty ++ ok
                } zip faultyAsOfs) groupBy (_._2)).toSeq sortBy (_._1) flatMap (_._2) unzip

              recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                asOfs,
                utopia)
              recordEventsInWorldWithoutGivingUpOnFailure(
                liftRecordings(mergedShuffledHistoryOverLotsOfThings.toStream),
                mergedAsOfs.toList,
                distopia)

              assert(utopia.nextRevision == distopia.nextRevision)
              assert(utopia.revisionAsOfs sameElements distopia.revisionAsOfs)

              val utopianScope =
                utopia.scopeFor(queryWhen, utopia.nextRevision)
              val distopianScope =
                distopia.scopeFor(queryWhen, distopia.nextRevision)

              val utopianHistory =
                historyFrom(utopia, recordingsGroupedById)(utopianScope)
              val distopianHistory =
                historyFrom(distopia, recordingsGroupedById)(distopianScope)

              ((utopianHistory.length == distopianHistory.length) :| s"${utopianHistory.length} == distopianHistory.length") && Prop
                .all(utopianHistory zip distopianHistory map {
                  case (utopianCase, distopianCase) =>
                    (utopianCase === distopianCase) :| s"${utopianCase} === distopianCase"
                }: _*)
          }
      })
    }

    it should "yield the same histories for scopes including all changes at the latest revision, regardless of how changes are grouped into revisions" in {
      val testCaseGenerator = for {
        worldOneWayResource     <- worldResourceGenerator
        worldAnotherWayResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        shuffledRecordingAndEventPairs = (shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
          random,
          recordingsGroupedById).zipWithIndex).toList
        bigShuffledHistoryOverLotsOfThingsOneWay = (random
          .splitIntoNonEmptyPieces(shuffledRecordingAndEventPairs))
          .force
        bigShuffledHistoryOverLotsOfThingsAnotherWay = (random
          .splitIntoNonEmptyPieces(shuffledRecordingAndEventPairs))
          .force
        asOfsOneWay <- Gen.listOfN(
          bigShuffledHistoryOverLotsOfThingsOneWay.length,
          instantGenerator) map (_.sorted)
        asOfsAnotherWay <- Gen.listOfN(
          bigShuffledHistoryOverLotsOfThingsAnotherWay.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldOneWayResource,
          worldAnotherWayResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThingsOneWay,
          bigShuffledHistoryOverLotsOfThingsAnotherWay,
          asOfsOneWay,
          asOfsAnotherWay,
          queryWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldOneWayResource,
        worldAnotherWayResource,
        recordingsGroupedById,
        bigShuffledHistoryOverLotsOfThingsOneWay,
        bigShuffledHistoryOverLotsOfThingsAnotherWay,
        asOfsOneWay,
        asOfsAnotherWay,
        queryWhen) =>
          worldOneWayResource and worldAnotherWayResource acquireAndGet {
            case (worldOneWay, worldAnotherWay) =>
              recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThingsOneWay),
                asOfsOneWay,
                worldOneWay)
              recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThingsAnotherWay),
                asOfsAnotherWay,
                worldAnotherWay)

              val scopeOneWay =
                worldOneWay.scopeFor(queryWhen, worldOneWay.nextRevision)
              val scopeAnotherWay =
                worldAnotherWay.scopeFor(queryWhen,
                  worldAnotherWay.nextRevision)

              val historyOneWay =
                historyFrom(worldOneWay, recordingsGroupedById)(scopeOneWay)
              val historyAnotherWay =
                historyFrom(worldAnotherWay, recordingsGroupedById)(
                  scopeAnotherWay)

              ((historyOneWay.length == historyAnotherWay.length) :| s"The number of datums calculated one way: ${historyOneWay.length} should be the same the other way: ${historyAnotherWay.length}, history on way: ${historyOneWay.toList}, the other way: ${historyAnotherWay.toList}") && Prop
                .all(historyOneWay zip historyAnotherWay map {
                  case (caseOneWay, caseAnotherWay) =>
                    (caseOneWay === caseAnotherWay) :| s"The datum calculated one way: ${caseOneWay} should be the same as the other way: ${caseAnotherWay}"
                }: _*)
          }
      })
    }

    val variablyTypedDataSamplesForAnIdGenerator =
      dataSamplesForAnIdGenerator_[FooHistory](
        fooHistoryIdGenerator,
        Gen.oneOf(moreSpecificFooHistoryDataSampleGenerator(faulty = false),
          fooHistoryDataSampleGenerator1(faulty = false)))

    val variablyTypedRecordingsGroupedByIdGenerator =
      recordingsGroupedByIdGenerator_(variablyTypedDataSamplesForAnIdGenerator,
        forbidAnnihilations = true)

    it should "allow events to vary in their view of the type of an item referenced by an id" in {
      {
        val testCaseGenerator = for {
          worldResource                 <- worldResourceGenerator
          recordingsGroupedById         <- variablyTypedRecordingsGroupedByIdGenerator
          obsoleteRecordingsGroupedById <- nonConflictingRecordingsGroupedByIdGenerator
          seed                          <- seedGenerator
          random = new Random(seed)
          shuffledRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById)
          shuffledObsoleteRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            obsoleteRecordingsGroupedById)
          bigShuffledHistoryOverLotsOfThings = intersperseObsoleteEvents(
            random,
            shuffledRecordings,
            shuffledObsoleteRecordings)
          asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
            instantGenerator) map (_.sorted)
        } yield
          (worldResource,
            recordingsGroupedById,
            bigShuffledHistoryOverLotsOfThings,
            asOfs)
        check(Prop.forAllNoShrink(testCaseGenerator) {
          case (worldResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs) =>
            worldResource acquireAndGet {
              world =>
                recordEventsInWorld(bigShuffledHistoryOverLotsOfThings,
                  asOfs,
                  world)

                val atTheEndOfTime = PositiveInfinity[Instant]

                val scope = world.scopeFor(atTheEndOfTime, world.nextRevision)

                val checks = for {
                  RecordingsNoLaterThan(
                  historyId,
                  historiesFrom,
                  pertinentRecordings,
                  _,
                  _) <- recordingsGroupedById flatMap (_.thePartNoLaterThan(
                    atTheEndOfTime))
                  Seq(history) = historiesFrom(scope)
                } yield
                  (historyId, history.datums, pertinentRecordings.map(_._1))

                if (checks.nonEmpty)
                  Prop.all(checks.map {
                    case (historyId, actualHistory, expectedHistory) =>
                      ((actualHistory.length == expectedHistory.length) :| s"For ${historyId}, expected to see: ${expectedHistory.length} datums, but got: ${actualHistory.length} - actual history: ${actualHistory.toList}, expected history: ${expectedHistory.toList}.") &&
                        Prop.all(
                          (actualHistory zip expectedHistory zipWithIndex) map {
                            case ((actual, expected), step) =>
                              (actual == expected) :| s"For ${historyId}, @step ${step}, the actual datum: ${actual} was not equal to the expected value: ${expected}."
                          }: _*)
                  }: _*)
                else Prop.undecided
            }
        })
      }
    }

    it should "forbid recording of events that have inconsistent views of the type of an item referenced by an id" in {
      {
        val testCaseGenerator = for {
          worldResource                 <- worldResourceGenerator
          recordingsGroupedById         <- variablyTypedRecordingsGroupedByIdGenerator
          obsoleteRecordingsGroupedById <- nonConflictingRecordingsGroupedByIdGenerator
          seed                          <- seedGenerator
          random = new Random(seed)
          shuffledRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById)
          shuffledObsoleteRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            obsoleteRecordingsGroupedById)
          bigShuffledHistoryOverLotsOfThings = intersperseObsoleteEvents(
            random,
            shuffledRecordings,
            shuffledObsoleteRecordings)
          asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
            instantGenerator) map (_.sorted)
          whenAnInconsistentEventOccurs <- unboundedInstantGenerator
        } yield
          (worldResource,
            recordingsGroupedById,
            bigShuffledHistoryOverLotsOfThings,
            asOfs,
            whenAnInconsistentEventOccurs)
        check(Prop.forAllNoShrink(testCaseGenerator) {
          case (worldResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          whenAnInconsistentEventOccurs) =>
            worldResource acquireAndGet {
              world =>
                var numberOfEvents = bigShuffledHistoryOverLotsOfThings.size

                val sizeOfPartOne = 1 min (numberOfEvents / 2)

                val (historyPartOne, historyPartTwo) = bigShuffledHistoryOverLotsOfThings splitAt sizeOfPartOne

                val (asOfsPartOne, asOfsPartTwo) = asOfs splitAt sizeOfPartOne

                recordEventsInWorld(historyPartOne, asOfsPartOne, world)

                val atTheEndOfTime = PositiveInfinity[Instant]

                val queryScope = world.scopeFor(whenAnInconsistentEventOccurs,
                  world.nextRevision)

                val eventIdForExtraConsistentChange  = -1
                val eventIdForInconsistentChangeOnly = -2

                for {
                  fooHistoryId: MoreSpecificFooHistory#Id <- recordingsGroupedById
                    .map(_.historyId) collect {
                    case id: MoreSpecificFooHistory#Id =>
                      id
                  }
                  if queryScope
                    .render(Bitemporal.withId[MoreSpecificFooHistory](
                      fooHistoryId)) exists (_.isInstanceOf[
                    MoreSpecificFooHistory])
                } {
                  intercept[RuntimeException] {
                    val consistentButLooselyTypedChange =
                      Change.forOneItem[FooHistory](fooHistoryId, {
                        fooHistory: FooHistory =>
                          fooHistory.property1 = "Hello"
                      })
                    val inconsistentlyTypedChange =
                      Change.forOneItem[AnotherSpecificFooHistory](
                        whenAnInconsistentEventOccurs)(
                        fooHistoryId
                          .asInstanceOf[AnotherSpecificFooHistory#Id], {
                          anotherSpecificFooHistory: AnotherSpecificFooHistory =>
                            anotherSpecificFooHistory.property3 = 6 // Have to actually *update* to cause an inconsistency.
                        }
                      )
                    world.revise(
                      Map(eventIdForExtraConsistentChange -> Some(
                        consistentButLooselyTypedChange),
                        eventIdForInconsistentChangeOnly -> Some(
                          inconsistentlyTypedChange)),
                      world.revisionAsOfs.last
                    )
                  }
                }

                recordEventsInWorld(historyPartTwo, asOfsPartTwo, world)

                val scope = world.scopeFor(atTheEndOfTime, world.nextRevision)

                val checks = for {
                  RecordingsNoLaterThan(
                  historyId,
                  historiesFrom,
                  pertinentRecordings,
                  _,
                  _) <- recordingsGroupedById flatMap (_.thePartNoLaterThan(
                    atTheEndOfTime))
                  Seq(history) = historiesFrom(scope)
                } yield
                  (historyId, history.datums, pertinentRecordings.map(_._1))

                if (checks.nonEmpty)
                  Prop.all(checks.map {
                    case (historyId, actualHistory, expectedHistory) =>
                      ((actualHistory.length == expectedHistory.length) :| s"For ${historyId}, expected to see: ${expectedHistory.length} datums, but got: ${actualHistory.length} - actual history: ${actualHistory.toList}, expected history: ${expectedHistory}.") &&
                        Prop.all(
                          (actualHistory zip expectedHistory zipWithIndex) map {
                            case ((actual, expected), step) =>
                              (actual == expected) :| s"For ${historyId}, @step ${step}, ${actual} was expected to be: ${expected}"
                          }: _*)
                  }: _*)
                else Prop.undecided
            }
        })
      }
    }

    def abstractedHistoryPositiveIntegerDataSampleGenerator(faulty: Boolean) =
      for { data <- Gen.posNum[Int] } yield
        (data,
          (when: americium.Unbounded[Instant],
           makeAChange: Boolean,
           abstractedHistoryId: AbstractedHistory#Id) =>
            eventConstructorReferringToOneItem[AbstractedHistory](makeAChange)(
              when)
              .apply(
                abstractedHistoryId,
                (abstractedHistory: AbstractedHistory) => {
                  // Neither changes nor measurements are allowed to read from the items they work on, with the exception of the 'id' property.
                  assert(abstractedHistoryId == abstractedHistory.id)
                  assertThrows[UnsupportedOperationException](
                    abstractedHistory.datums)
                  assertThrows[UnsupportedOperationException](
                    abstractedHistory.property)

                  if (faulty) abstractedHistory.forceInvariantBreakage() // Modelling breakage of the bitemporal invariant.

                  abstractedHistory.property = data
                }
              ))

    def implementingHistoryNegativeIntegerDataSampleGenerator(faulty: Boolean) =
      for { data <- Gen.negNum[Int] } yield
        (data,
          (when: americium.Unbounded[Instant],
           makeAChange: Boolean,
           implementingHistoryId: ImplementingHistory#Id) =>
            eventConstructorReferringToOneItem[ImplementingHistory](makeAChange)(
              when)
              .apply(
                implementingHistoryId,
                (implementingHistory: ImplementingHistory) => {
                  // Neither changes nor measurements are allowed to read from the items they work on, with the exception of the 'id' property.
                  assert(implementingHistoryId == implementingHistory.id)
                  assertThrows[UnsupportedOperationException](
                    implementingHistory.datums)
                  assertThrows[UnsupportedOperationException](
                    implementingHistory.property)

                  if (faulty) implementingHistory.forceInvariantBreakage() // Modelling breakage of the bitemporal invariant.

                  implementingHistory.property = data
                }
              ))

    val abstractedDataSamplesForAnIdGenerator =
      dataSamplesForAnIdGenerator_[AbstractedHistory](
        abstractedOrImplementingHistoryIdGenerator,
        abstractedHistoryPositiveIntegerDataSampleGenerator(faulty = false)
      )
    val implementingDataSamplesForAnIdGenerator =
      dataSamplesForAnIdGenerator_[ImplementingHistory](
        abstractedOrImplementingHistoryIdGenerator,
        implementingHistoryNegativeIntegerDataSampleGenerator(faulty = false)
      )

    it should "allow events to refer to an item via an abstract type provided it is defined concretely in other events" in {
      {
        val testCaseGenerator = for {
          worldResource <- worldResourceGenerator
          recordingsForAbstractedHistoriesGroupedById <- recordingsGroupedByIdGenerator_(
            abstractedDataSamplesForAnIdGenerator,
            forbidAnnihilations = true,
            forbidMeasurements = false)
          idsForAbstractedHistories = recordingsForAbstractedHistoriesGroupedById
            .map(_.historyId)
            .toSet
          recordingsForImplementingHistoriesGroupedById <- recordingsGroupedByIdGenerator_(
            implementingDataSamplesForAnIdGenerator,
            forbidAnnihilations = true,
            forbidMeasurements = true)
          idsForImplementingHistories = recordingsForImplementingHistoriesGroupedById
            .map(_.historyId)
            .toSet
          sharedIds = idsForAbstractedHistories intersect idsForImplementingHistories
          if sharedIds.nonEmpty
          relevantRecordingsForAbstractedHistoriesGroupedById = recordingsForAbstractedHistoriesGroupedById filter (
            recordings => sharedIds.contains(recordings.historyId))
          relevantRecordingsForImplementedHistoriesGroupedById = recordingsForImplementingHistoriesGroupedById filter (
            recordings => sharedIds.contains(recordings.historyId))
          seed <- seedGenerator
          random = new Random(seed)
          shuffledRecordingsForAbstractedHistories = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            relevantRecordingsForAbstractedHistoriesGroupedById)
          shuffledRecordingsForImplementingHistories = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            relevantRecordingsForImplementedHistoriesGroupedById)
          bigShuffledHistoryOverLotsOfThingsThatMakesSureThatAtLeastOneImplementingHistoryGetsBookedInFirst = liftRecordings(
            random
              .splitIntoNonEmptyPieces(
                (shuffledRecordingsForImplementingHistories ++ shuffledRecordingsForAbstractedHistories).zipWithIndex))
          asOfs <- Gen.listOfN(
            bigShuffledHistoryOverLotsOfThingsThatMakesSureThatAtLeastOneImplementingHistoryGetsBookedInFirst.length,
            instantGenerator) map (_.sorted)
          queryWhen <- unboundedInstantGenerator
        } yield
          (worldResource,
            bigShuffledHistoryOverLotsOfThingsThatMakesSureThatAtLeastOneImplementingHistoryGetsBookedInFirst,
            asOfs,
            queryWhen)
        check(Prop.forAllNoShrink(testCaseGenerator) {
          case (worldResource,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen) =>
            worldResource acquireAndGet { world =>
              recordEventsInWorld(bigShuffledHistoryOverLotsOfThings,
                asOfs,
                world)

              Prop.proved
            }
        })
      }
    }

    it should "allow events to refer to an item via a concrete type when it is defined abstractly in other events" in {
      {
        val testCaseGenerator = for {
          worldResource <- worldResourceGenerator
          recordingsForAbstractedHistoriesGroupedById <- recordingsGroupedByIdGenerator_(
            abstractedDataSamplesForAnIdGenerator,
            forbidAnnihilations = true,
            forbidMeasurements = true)
          idsForAbstractedHistories = recordingsForAbstractedHistoriesGroupedById
            .map(_.historyId)
            .toSet
          recordingsForImplementingHistoriesGroupedById <- recordingsGroupedByIdGenerator_(
            implementingDataSamplesForAnIdGenerator,
            forbidAnnihilations = true,
            forbidMeasurements = false)
          idsForImplementingHistories = recordingsForImplementingHistoriesGroupedById
            .map(_.historyId)
            .toSet
          sharedIds = idsForAbstractedHistories intersect idsForImplementingHistories
          if sharedIds.nonEmpty
          relevantRecordingsForAbstractedHistoriesGroupedById = recordingsForAbstractedHistoriesGroupedById filter (
            recordings => sharedIds.contains(recordings.historyId))
          relevantRecordingsForImplementedHistoriesGroupedById = recordingsForImplementingHistoriesGroupedById filter (
            recordings => sharedIds.contains(recordings.historyId))
          seed <- seedGenerator
          random = new Random(seed)
          shuffledRecordingsForAbstractedHistories = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            relevantRecordingsForAbstractedHistoriesGroupedById)
          shuffledRecordingsForImplementingHistories = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            relevantRecordingsForImplementedHistoriesGroupedById)
          bigShuffledHistoryOverLotsOfThingsThatMakesSureThatAtLeastOneImplementingHistoryGetsBookedInFirst = liftRecordings(
            random
              .splitIntoNonEmptyPieces(
                (shuffledRecordingsForImplementingHistories ++ shuffledRecordingsForAbstractedHistories).zipWithIndex))
          asOfs <- Gen.listOfN(
            bigShuffledHistoryOverLotsOfThingsThatMakesSureThatAtLeastOneImplementingHistoryGetsBookedInFirst.length,
            instantGenerator) map (_.sorted)
          queryWhen <- unboundedInstantGenerator
        } yield
          (worldResource,
            bigShuffledHistoryOverLotsOfThingsThatMakesSureThatAtLeastOneImplementingHistoryGetsBookedInFirst,
            asOfs,
            queryWhen)
        check(Prop.forAllNoShrink(testCaseGenerator) {
          case (worldResource,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen) =>
            worldResource acquireAndGet { world =>
              recordEventsInWorld(bigShuffledHistoryOverLotsOfThings,
                asOfs,
                world)

              Prop.proved
            }
        })
      }
    }

    val mixedAbstractedAndImplementingDataSamplesForAnIdGenerator =
      dataSamplesForAnIdGenerator_[AbstractedHistory](
        abstractedOrImplementingHistoryIdGenerator,
        Gen.frequency(
          1 -> abstractedHistoryPositiveIntegerDataSampleGenerator(
            faulty = false),
          3 -> implementingHistoryNegativeIntegerDataSampleGenerator(
            faulty = false))
      )

    it should "record bookings from events using abstract types in the same manner as for concrete types" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator_(
          mixedAbstractedAndImplementingDataSamplesForAnIdGenerator,
          forbidAnnihilations = true,
          forbidMeasurements = false)
        obsoleteRecordingsGroupedById <- nonConflictingRecordingsGroupedByIdGenerator
        seed                          <- seedGenerator
        random = new Random(seed)
        shuffledRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
          random,
          recordingsGroupedById)
        shuffledObsoleteRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
          random,
          obsoleteRecordingsGroupedById)
        bigShuffledHistoryOverLotsOfThings = intersperseObsoleteEvents(
          random,
          shuffledRecordings,
          shuffledObsoleteRecordings)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen,
          seed)
      check(
        Prop.forAllNoShrink(testCaseGenerator) {
          case (worldResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen,
          seed) =>
            worldResource acquireAndGet { world =>
              try {
                recordEventsInWorld(bigShuffledHistoryOverLotsOfThings,
                  asOfs,
                  world)

                val scope =
                  world.scopeFor(queryWhen, world.nextRevision)

                val checks = for {
                  RecordingsNoLaterThan(
                  historyId,
                  historiesFrom,
                  pertinentRecordings,
                  _,
                  _) <- recordingsGroupedById flatMap (_.thePartNoLaterThan(
                    queryWhen))
                  Seq(history) = historiesFrom(scope)
                  haveBothAnAbstractedAndAnImplementingDatumSomewhere = history.datums
                    .exists { case datum: Int => datum > 0 } && history.datums
                    .exists { case datum: Int => datum < 0 }
                  if haveBothAnAbstractedAndAnImplementingDatumSomewhere
                } yield
                  (historyId, history.datums, pertinentRecordings.map(_._1))

                checks.nonEmpty ==>
                  Prop.all(checks.map {
                    case (historyId, actualHistory, expectedHistory) =>
                      ((actualHistory.length == expectedHistory.length) :| s"For ${historyId}, expected to see: ${expectedHistory.length} datums, but got: ${actualHistory.length} - actual history: ${actualHistory.toList}, expected history: ${expectedHistory}.") &&
                        Prop.all(
                          (actualHistory zip expectedHistory zipWithIndex) map {
                            case ((actual, expected), step) =>
                              (actual == expected) :| s"For ${historyId}, @step ${step}, ${actual} was expected to be: ${expected}"
                          }: _*)
                  }: _*)
              } catch {
                case _: UnsupportedOperationException => Prop.undecided
              }
            }
        },
        minSuccessful(10)
      )
    }

    it should "forbid the booking of events that only ever refer to an item via an abstract type" in {
      {
        val testCaseGenerator = for {
          worldResource <- worldResourceGenerator
          recordingsForAbstractedHistoriesGroupedById <- recordingsGroupedByIdGenerator_(
            abstractedDataSamplesForAnIdGenerator,
            forbidAnnihilations = true,
            forbidMeasurements = false)
          obsoleteRecordingsGroupedById <- nonConflictingRecordingsGroupedByIdGenerator
          seed                          <- seedGenerator
          random = new Random(seed)
          shuffledRecordingsForAbstractedHistories = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsForAbstractedHistoriesGroupedById)
          shuffledObsoleteRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            obsoleteRecordingsGroupedById)
          bigShuffledHistoryOverLotsOfThings = intersperseObsoleteEvents(
            random,
            shuffledRecordingsForAbstractedHistories,
            shuffledObsoleteRecordings)
          asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
            instantGenerator) map (_.sorted)
          queryWhen <- unboundedInstantGenerator
        } yield
          (worldResource,
            recordingsForAbstractedHistoriesGroupedById,
            bigShuffledHistoryOverLotsOfThings,
            asOfs,
            queryWhen)
        check(Prop.forAllNoShrink(testCaseGenerator) {
          case (worldResource,
          recordingsForAbstractedHistoriesGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen) =>
            worldResource acquireAndGet { world =>
              intercept[UnsupportedOperationException] {
                recordEventsInWorld(bigShuffledHistoryOverLotsOfThings,
                  asOfs,
                  world)
              }

              Prop.proved
            }
        })
      }
    }

    it should "take into account the precise type of the item referenced by an annihilation when other events refer to the same item via looser types" in {
      {
        val testCaseGenerator = for {
          worldResource <- worldResourceGenerator
          recordingsGroupedById <- recordingsGroupedByIdGenerator_(
            mixedAbstractedAndImplementingDataSamplesForAnIdGenerator,
            forbidAnnihilations = true,
            forbidMeasurements = false)
          obsoleteRecordingsGroupedById <- nonConflictingRecordingsGroupedByIdGenerator
          seed                          <- seedGenerator
          random = new Random(seed)
          shuffledRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById)
          shuffledObsoleteRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            obsoleteRecordingsGroupedById)
          bigShuffledHistoryOverLotsOfThings = intersperseObsoleteEvents(
            random,
            shuffledRecordings,
            shuffledObsoleteRecordings)
          asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
            instantGenerator) map (_.sorted)
          whenAnAnnihilationOccurs     <- instantGenerator
          queryWhenLeadsAnnihilationBy <- Gen.posNum[Int]
        } yield
          (worldResource,
            recordingsGroupedById,
            bigShuffledHistoryOverLotsOfThings,
            asOfs,
            whenAnAnnihilationOccurs,
            whenAnAnnihilationOccurs minusMillis queryWhenLeadsAnnihilationBy)
        check(Prop.forAllNoShrink(testCaseGenerator) {
          case (worldResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          whenAnAnnihilationOccurs,
          queryWhen) =>
            worldResource acquireAndGet { world =>
              try {
                recordEventsInWorld(bigShuffledHistoryOverLotsOfThings,
                  asOfs,
                  world)

                for {
                  (RecordingsNoLaterThan(historyId, _, _, _, _),
                  eventIdForAnnihilation) <- recordingsGroupedById flatMap (_.thePartNoLaterThan(
                    Finite(queryWhen))) zip Stream.from(-1, -1)
                } {
                  world.revise(eventIdForAnnihilation,
                    Annihilation[NegatingImplementingHistory](
                      whenAnAnnihilationOccurs,
                      historyId.asInstanceOf[String]),
                    world.revisionAsOfs.last)
                }

                val scopeThatShouldPickOutNegatingImplementingHistories =
                  world.scopeFor(Finite(queryWhen), world.nextRevision)

                for {
                  (RecordingsNoLaterThan(historyId, _, _, _, _),
                  eventIdForAnnihilation) <- recordingsGroupedById flatMap (_.thePartNoLaterThan(
                    Finite(queryWhen))) zip Stream.from(-1, -1)
                } {
                  world.revise(eventIdForAnnihilation,
                    Annihilation[ImplementingHistory](
                      whenAnAnnihilationOccurs,
                      historyId.asInstanceOf[String]),
                    world.revisionAsOfs.last)
                }

                val scopeThatShouldPickOutImplementingHistories =
                  world.scopeFor(Finite(queryWhen), world.nextRevision)

                val checks = for {
                  RecordingsNoLaterThan(historyId, historiesFrom, _, _, _) <- recordingsGroupedById flatMap (_.thePartNoLaterThan(
                    Finite(queryWhen)))
                  Seq(negatedHistory) = historiesFrom(
                    scopeThatShouldPickOutNegatingImplementingHistories)
                  Seq(history) = historiesFrom(
                    scopeThatShouldPickOutImplementingHistories)
                } yield (historyId, negatedHistory.datums, history.datums)

                if (checks.nonEmpty)
                  Prop.all(checks.map {
                    case (historyId, negatedHistory, expectedHistory) =>
                      ((negatedHistory.length == expectedHistory.length) :| s"For ${historyId}, ${negatedHistory.length} == expectedHistory.length") &&
                        Prop.all(
                          (negatedHistory zip expectedHistory zipWithIndex) map {
                            case ((actual, expected), step) =>
                              val negatedExpectedValue =
                                -expected.asInstanceOf[Int]
                              (actual == negatedExpectedValue) :| s"For ${historyId}, @step ${step}, ${actual} == ${negatedExpectedValue}"
                          }: _*)
                  }: _*)
                else Prop.undecided
              } catch {
                case _: UnsupportedOperationException => Prop.undecided
              }
            }
        })
      }
    }

    it should "reflect the absence of all items of a compatible type relevant to a scope that share an id following an annihilation using that id." in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = true)
        queryWhen <- instantGenerator
        possiblyEmptySetOfIdsThatEachReferToMoreThanOneItem = ((recordingsGroupedById flatMap (_.thePartNoLaterThan(
          Finite(queryWhen))) map (_.historyId)) groupBy identity collect {
          case (id, group) if 1 < group.size => id
        }).toSet
        idsThatEachReferToMoreThanOneItem <- Gen.const(
          possiblyEmptySetOfIdsThatEachReferToMoreThanOneItem)
        if possiblyEmptySetOfIdsThatEachReferToMoreThanOneItem.nonEmpty
        obsoleteRecordingsGroupedById <- nonConflictingRecordingsGroupedByIdGenerator
        seed                          <- seedGenerator
        random = new Random(seed)
        shuffledRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
          random,
          recordingsGroupedById)
        shuffledObsoleteRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
          random,
          obsoleteRecordingsGroupedById)
        bigShuffledHistoryOverLotsOfThings = intersperseObsoleteEvents(
          random,
          shuffledRecordings,
          shuffledObsoleteRecordings)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
      } yield
        (worldResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen,
          idsThatEachReferToMoreThanOneItem)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        recordingsGroupedById,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        queryWhen,
        idsThatEachReferToMoreThanOneItem) =>
          worldResource acquireAndGet {
            world =>
              recordEventsInWorld(bigShuffledHistoryOverLotsOfThings,
                asOfs,
                world)

              val maximumEventId =
                bigShuffledHistoryOverLotsOfThings.flatten map (_._2) max

              val annihilationEvents =
                idsThatEachReferToMoreThanOneItem.zipWithIndex map {
                  case (idThatEachRefersToMoreThanOneItem, index) =>
                    (1 + maximumEventId + index,
                      Some(
                        Annihilation[History](queryWhen,
                          idThatEachRefersToMoreThanOneItem
                            .asInstanceOf[History#Id])))
                } toMap

              world.revise(annihilationEvents, asOfs.last)

              val scope = world.scopeFor(Finite(queryWhen), world.nextRevision)

              Prop.all(idsThatEachReferToMoreThanOneItem.toSeq map (id => {
                val itemsThatShouldNotExist = scope
                  .render(
                    Bitemporal.withId[History](id.asInstanceOf[History#Id]))
                  .toList
                itemsThatShouldNotExist.isEmpty :| s"The items '$itemsThatShouldNotExist' for id: '$id' should not exist at the query time of: '$queryWhen'."
              }): _*)
          }
      })
    }
  }

  def worldWithEventsThatHaveSinceBeenCorrectedBehaviour = {
    it should "extend the history of an item whose annihilation is annulled to pick up any subsequent events relating to that item." in {
      val itemId = "Fred"

      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        eventTimes    <- Gen.nonEmptyListOf(instantGenerator) map (_.sorted)
        annihilationWhen <- instantGenerator filter (when =>
          when.isAfter(eventTimes.head) && !when.isAfter(eventTimes.last))
        steps = 1 to eventTimes.size
        recordings: List[(Unbounded[Instant], Event)] = eventTimes zip steps map {
          case (when, step) =>
            Finite(when) -> Change
              .forOneItem[IntegerHistory](when)(itemId, {
              item: IntegerHistory =>
                item.integerProperty = step
            })
        }
        seed <- seedGenerator
        random = new Random(seed)
        shuffledRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhenForAGivenItem(
          random,
          recordings)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffledRecordings.zipWithIndex)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
      } yield
        (worldResource,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          steps,
          annihilationWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        steps,
        annihilationWhen) =>
          worldResource acquireAndGet {
            world =>
              val initialEventId = -2

              world.revise(
                initialEventId,
                Change.forOneItem[IntegerHistory](annihilationWhen)(itemId, {
                  item: IntegerHistory =>
                    item.integerProperty = -1
                }),
                asOfs.head)

              val annihilationEventId = -1

              world.revise(annihilationEventId,
                Annihilation[Any](annihilationWhen, itemId),
                asOfs.head)

              recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                asOfs,
                world)

              world.annul(initialEventId, asOfs.last)

              world.annul(annihilationEventId, asOfs.last)

              val scope =
                world.scopeFor(PositiveInfinity[Instant](), world.nextRevision)

              val fredTheItem = scope
                .render(Bitemporal.withId[IntegerHistory](itemId))
                .force

              (steps == fredTheItem.head.datums) :| s"Expecting: ${steps}, but got: ${fredTheItem.head.datums}"
          }
      })
    }

    it should "build an item's state in a manner consistent with the history experienced by the item regardless of any corrected history." in {
      val itemId = "Fred"

      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        eventTimes    <- Gen.nonEmptyListOf(instantGenerator) map (_.sorted)
        steps = 1 to eventTimes.size
        recordings: List[(Unbounded[Instant], Event)] = eventTimes zip steps map {
          case (when, step) =>
            Finite(when) -> Change
              .forOneItem[IntegerHistory](when)(itemId, {
              item: IntegerHistory =>
                item.integerProperty = step
            })
        }
        obsoleteEventTimes <- Gen.nonEmptyListOf(instantGenerator)
        obsoleteSteps = 1 to obsoleteEventTimes.size
        obsoleteRecordings: List[(Unbounded[Instant], Event)] = obsoleteEventTimes zip obsoleteSteps map {
          case (when, step) =>
            Finite(when) -> Change
              .forOneItem[IntegerHistory](when)(itemId, {
              item: IntegerHistory =>
                item.integerProperty = step
            })
        }
        seed <- seedGenerator
        random = new Random(seed)
        shuffledRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhenForAGivenItem(
          random,
          recordings)
        shuffledObsoleteRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhenForAGivenItem(
          random,
          obsoleteRecordings)
        bigShuffledHistoryOverLotsOfThings = intersperseObsoleteEvents(
          random,
          shuffledRecordings,
          shuffledObsoleteRecordings)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
      } yield (worldResource, bigShuffledHistoryOverLotsOfThings, asOfs, steps)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        steps) =>
          worldResource acquireAndGet {
            world =>
              recordEventsInWorld(bigShuffledHistoryOverLotsOfThings,
                asOfs,
                world)

              val scope =
                world.scopeFor(PositiveInfinity[Instant](), world.nextRevision)

              val fredTheItem = scope
                .render(Bitemporal.withId[IntegerHistory](itemId))
                .force

              (steps == fredTheItem.head.datums) :| s"Expecting: ${steps}, but got: ${fredTheItem.head.datums}"
          }
      })
    }

    it should "build an item's state in a manner consistent with the history experienced by the item regardless of any corrected history - with a twist." in {
      val itemId = "Fred"

      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        eventTimes    <- Gen.nonEmptyListOf(instantGenerator) map (_.sorted)
        steps = 1 to eventTimes.size
        recordings: List[(Unbounded[Instant], Event)] = eventTimes zip steps map {
          case (when, step) =>
            Finite(when) -> Change
              .forOneItem[IntegerHistory](when)(itemId, {
              item: IntegerHistory =>
                item.integerProperty = step
            })
        }
        seed <- seedGenerator
        random             = new Random(seed)
        obsoleteEventTimes = random.shuffle(eventTimes)
        obsoleteSteps      = 1 to obsoleteEventTimes.size
        obsoleteRecordings: List[(Unbounded[Instant], Event)] = obsoleteEventTimes zip obsoleteSteps map {
          case (when, step) =>
            Finite(when) -> Change
              .forOneItem[IntegerHistory](when)(itemId, {
              item: IntegerHistory =>
                item.integerProperty = step
            })
        }

        shuffledRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhenForAGivenItem(
          random,
          recordings)
        shuffledObsoleteRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhenForAGivenItem(
          random,
          recordings)
        bigShuffledHistoryOverLotsOfThings = intersperseObsoleteEvents(
          random,
          shuffledRecordings,
          shuffledObsoleteRecordings)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
      } yield (worldResource, bigShuffledHistoryOverLotsOfThings, asOfs, steps)
      check(
        Prop.forAllNoShrink(testCaseGenerator) {
          case (worldResource,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          steps) =>
            worldResource acquireAndGet {
              world =>
                recordEventsInWorld(bigShuffledHistoryOverLotsOfThings,
                  asOfs,
                  world)

                val scope =
                  world.scopeFor(PositiveInfinity[Instant](),
                    world.nextRevision)

                val fredTheItem = scope
                  .render(Bitemporal.withId[IntegerHistory](itemId))
                  .force

                (steps == fredTheItem.head.datums) :| s"Expecting: ${steps}, but got: ${fredTheItem.head.datums}"
            }
        },
        MinSuccessful(400)
      )
    }

    it should "build an item's state in a manner consistent with the history experienced by the item regardless of any corrected history - with another twist." in {
      val itemId = "Fred"

      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        eventTimes <- Gen.chooseNum(0L, 50L) map (0L to _ toList) map (_.map(
          timeInSeconds => Instant.ofEpochSecond(24 * 60 * 60 * timeInSeconds)))
        steps = 1 to eventTimes.size
        recordings: List[(Unbounded[Instant], Event)] = eventTimes zip steps map {
          case (when, step) =>
            Finite(when) -> Change
              .forOneItem[IntegerHistory](when)(itemId, {
              item: IntegerHistory =>
                item.integerProperty = step
            })
        }
        seed <- seedGenerator
        random             = new Random(seed)
        obsoleteEventTimes = random.shuffle(eventTimes)
        obsoleteSteps      = 1 to obsoleteEventTimes.size
        obsoleteRecordings: List[(Unbounded[Instant], Event)] = obsoleteEventTimes zip obsoleteSteps map {
          case (when, step) =>
            Finite(when) -> Change
              .forOneItem[IntegerHistory](when)(itemId, {
              item: IntegerHistory =>
                item.integerProperty = step
            })
        }

        pairsOfObsoleteAndSucceedingEvents = obsoleteRecordings.zipWithIndex zip recordings.zipWithIndex

        historyOverLotsOfThings = pairsOfObsoleteAndSucceedingEvents flatMap {
          case (obsolete, succeeding) => Seq(Seq(obsolete), Seq(succeeding))
        } toStream

        asOfs <- Gen.listOfN(historyOverLotsOfThings.length, instantGenerator) map (_.sorted)
      } yield (worldResource, historyOverLotsOfThings, asOfs, steps)
      check(
        Prop.forAllNoShrink(testCaseGenerator) {
          case (worldResource, historyOverLotsOfThings, asOfs, steps) =>
            worldResource acquireAndGet {
              world =>
                recordEventsInWorld(liftRecordings(historyOverLotsOfThings),
                  asOfs,
                  world)

                val scope =
                  world.scopeFor(PositiveInfinity[Instant](),
                    world.nextRevision)

                val fredTheItem = scope
                  .render(Bitemporal.withId[IntegerHistory](itemId))
                  .force

                (steps.toSet == fredTheItem.head.datums.toSet) :| s"Expecting: ${steps}, but got: ${fredTheItem.head.datums}"
            }
        },
        MinSuccessful(700)
      )
    }

    it should "yield a history at the final revision based only on the latest corrections" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        obsoleteRecordingsGroupedById <- nonConflictingRecordingsGroupedByIdGenerator
        seed                          <- seedGenerator
        random = new Random(seed)
        shuffledRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
          random,
          recordingsGroupedById)
        shuffledObsoleteRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
          random,
          obsoleteRecordingsGroupedById)
        bigShuffledHistoryOverLotsOfThings = intersperseObsoleteEvents(
          random,
          shuffledRecordings,
          shuffledObsoleteRecordings)
        asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        recordingsGroupedById,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        queryWhen) =>
          worldResource acquireAndGet {
            world =>
              recordEventsInWorld(bigShuffledHistoryOverLotsOfThings,
                asOfs,
                world)

              val scope = world.scopeFor(queryWhen, world.nextRevision)

              val checks = for {
                RecordingsNoLaterThan(
                historyId,
                historiesFrom,
                pertinentRecordings,
                _,
                _) <- recordingsGroupedById flatMap (_.thePartNoLaterThan(
                  queryWhen))
                Seq(history) = historiesFrom(scope)
              } yield (historyId, history.datums, pertinentRecordings.map(_._1))

              if (checks.nonEmpty)
                Prop.all(checks.map {
                  case (historyId, actualHistory, expectedHistory) =>
                    ((actualHistory.length == expectedHistory.length) :| s"For ${historyId}, expected to see: ${expectedHistory.length} datums, but got: ${actualHistory.length} - actual history: ${actualHistory.toList}, expected history: ${expectedHistory}.") &&
                      Prop.all(
                        (actualHistory zip expectedHistory zipWithIndex) map {
                          case ((actual, expected), step) =>
                            (actual == expected) :| s"For ${historyId}, @step ${step}, ${actual} was expected to be ${expected}"
                        }: _*)
                }: _*)
              else Prop.undecided
          }
      })
    }

    it should "allow an entire history to be completely annulled and then rewritten" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = true)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = intersperseObsoleteEvents
          .chunkKeepingEventIdsUniquePerChunk(
            random,
            shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
              random,
              recordingsGroupedById).zipWithIndex)
        allEventIds    = bigShuffledHistoryOverLotsOfThings flatMap (_ map (_._2))
        maximumEventId = allEventIds.max
        eventIdsThatMayBeSpuriousAndDuplicated = allEventIds ++
          random.chooseSeveralOf(
            allEventIds,
            random.chooseAnyNumberFromZeroToOneLessThan(allEventIds.length)) ++
          (1 + maximumEventId to 10 + maximumEventId)
        annulmentsGalore = intersperseObsoleteEvents
          .chunkKeepingEventIdsUniquePerChunk(
            random,
            random
              .shuffle(eventIdsThatMayBeSpuriousAndDuplicated) map ((None: Option[
              (Unbounded[Instant], Event)]) -> _))
        historyLength    = bigShuffledHistoryOverLotsOfThings.length
        annulmentsLength = annulmentsGalore.length
        asOfs <- Gen.listOfN(2 * historyLength + annulmentsLength,
          instantGenerator) map (_.sorted)
        (asOfsForFirstHistory, remainingAsOfs)      = asOfs splitAt historyLength
        (asOfsForAnnulments, asOfsForSecondHistory) = remainingAsOfs splitAt annulmentsLength
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          annulmentsGalore,
          asOfsForFirstHistory,
          asOfsForAnnulments,
          asOfsForSecondHistory,
          queryWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        recordingsGroupedById,
        bigShuffledHistoryOverLotsOfThings,
        annulmentsGalore,
        asOfsForFirstHistory,
        asOfsForAnnulments,
        asOfsForSecondHistory,
        queryWhen) =>
          worldResource acquireAndGet {
            world =>
              // Define a history the first time around...

              recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                asOfsForFirstHistory,
                world)

              val scopeForFirstHistory =
                world.scopeFor(queryWhen, world.nextRevision)

              val firstHistory =
                historyFrom(world, recordingsGroupedById)(scopeForFirstHistory)

              // Annul that history completely...

              recordEventsInWorld(annulmentsGalore, asOfsForAnnulments, world)

              val scopeAfterAnnulments =
                world.scopeFor(queryWhen, world.nextRevision)

              val historyAfterAnnulments =
                historyFrom(world, recordingsGroupedById)(scopeAfterAnnulments)

              // ...and then recreate what should be the same history.

              recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                asOfsForSecondHistory,
                world)

              val scopeForSecondHistory =
                world.scopeFor(queryWhen, world.nextRevision)

              val secondHistory =
                historyFrom(world, recordingsGroupedById)(scopeForSecondHistory)

              (historyAfterAnnulments.isEmpty :| s"${historyAfterAnnulments}.isEmpty") &&
                ((firstHistory == secondHistory) :| s"firstHistory === ${secondHistory}")
          }
      })
    }

    it should "yield a history whose versions of events reflect the revision of a scope made from it" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = true)
        obsoleteRecordingsGroupedById  <- nonConflictingRecordingsGroupedByIdGenerator
        followingRecordingsGroupedById <- nonConflictingRecordingsGroupedByIdGenerator
        seed                           <- seedGenerator
        random = new Random(seed)
        shuffledRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
          random,
          recordingsGroupedById)
        shuffledObsoleteRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
          random,
          obsoleteRecordingsGroupedById)
        bigShuffledHistoryOverLotsOfThings = intersperseObsoleteEvents(
          random,
          shuffledRecordings,
          shuffledObsoleteRecordings)
        shuffledFollowingRecordingAndEventPairs = (shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
          random,
          followingRecordingsGroupedById).zipWithIndex).toList
        bigFollowingShuffledHistoryOverLotsOfThings = random
          .splitIntoNonEmptyPieces(shuffledFollowingRecordingAndEventPairs)
        bigOverallShuffledHistoryOverLotsOfThings = bigShuffledHistoryOverLotsOfThings ++ liftRecordings(
          bigFollowingShuffledHistoryOverLotsOfThings)
        asOfs <- Gen.listOfN(bigOverallShuffledHistoryOverLotsOfThings.length,
          instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
        revisionOffsetToCheckAt = bigShuffledHistoryOverLotsOfThings.length
      } yield
        (worldResource,
          recordingsGroupedById,
          bigOverallShuffledHistoryOverLotsOfThings,
          asOfs,
          queryWhen,
          revisionOffsetToCheckAt)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        recordingsGroupedById,
        bigOverallShuffledHistoryOverLotsOfThings,
        asOfs,
        queryWhen,
        revisionOffsetToCheckAt) =>
          worldResource acquireAndGet {
            world =>
              recordEventsInWorld(bigOverallShuffledHistoryOverLotsOfThings,
                asOfs,
                world)

              val scope =
                world.scopeFor(queryWhen,
                  World.initialRevision + revisionOffsetToCheckAt)

              val checks = for {
                RecordingsNoLaterThan(
                historyId,
                historiesFrom,
                pertinentRecordings,
                _,
                _) <- recordingsGroupedById flatMap (_.thePartNoLaterThan(
                  queryWhen))
                Seq(history) = historiesFrom(scope)
              } yield (historyId, history.datums, pertinentRecordings.map(_._1))

              if (checks.nonEmpty)
                Prop.all(checks.map {
                  case (historyId, actualHistory, expectedHistory) =>
                    ((actualHistory.length == expectedHistory.length) :| s"For ${historyId}, expected to see: ${expectedHistory.length} datums, but got: ${actualHistory.length} - actual history: ${actualHistory.toList}, expected history: ${expectedHistory}.") &&
                      Prop.all(
                        (actualHistory zip expectedHistory zipWithIndex) map {
                          case ((actual, expected), step) =>
                            (actual == expected) :| s"For ${historyId}, @step ${step}, ${actual} was expected to be: ${expected}"
                        }: _*)
                }: _*)
              else Prop.undecided
          }
      })
    }

    it should "yield a history whose versions of events reflect arbitrary scopes made from it at varying revisions" in {
      val testCaseSubSectionGenerator = for {
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        obsoleteRecordingsGroupedById <- nonConflictingRecordingsGroupedByIdGenerator
        seed                          <- seedGenerator
        random = new Random(seed)
        shuffledRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
          random,
          recordingsGroupedById)
        shuffledObsoleteRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
          random,
          obsoleteRecordingsGroupedById)
        bigShuffledHistoryOverLotsOfThings = intersperseObsoleteEvents(
          random,
          shuffledRecordings,
          shuffledObsoleteRecordings)
      } yield (recordingsGroupedById, bigShuffledHistoryOverLotsOfThings)

      val testCaseGenerator = for {
        worldResource       <- worldResourceGenerator
        testCaseSubsections <- Gen.listOfN(4, testCaseSubSectionGenerator)
        asOfs <- Gen.listOfN(testCaseSubsections map (_._2.length) sum,
          instantGenerator) map (_.sorted)
        asOfsForSubsections = stream.unfold(testCaseSubsections -> asOfs) {
          case ((testCaseSubsection :: remainingTestCaseSubsections), asOfs) =>
            val numberOfRevisions                    = testCaseSubsection._2.length
            val (asOfsForSubsection, remainingAsOfs) = asOfs splitAt numberOfRevisions
            Some(asOfsForSubsection,
              remainingTestCaseSubsections -> remainingAsOfs)
          case _ => None
        }
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource, testCaseSubsections, asOfsForSubsections, queryWhen)
      check(
        Prop.forAllNoShrink(testCaseGenerator) {
          case (worldResource,
          testCaseSubsections,
          asOfsForSubsections,
          queryWhen) =>
            worldResource acquireAndGet {
              world =>
                val listOfRevisionsToCheckAtAndRecordingsGroupedById =
                  stream.unfold(
                    (testCaseSubsections zip asOfsForSubsections) -> -1) {
                    case ((((recordingsGroupedById,
                    bigShuffledHistoryOverLotsOfThings),
                    asOfs) :: remainingSubsections),
                    maximumEventIdFromPreviousSubsection) =>
                      val sortedEventIds =
                        (bigShuffledHistoryOverLotsOfThings flatMap (_ map (_._2))).sorted.distinct
                      assert(0 == sortedEventIds.head)
                      assert((sortedEventIds zip sortedEventIds.tail).forall {
                        case (first, second) => 1 + first == second
                      })
                      val maximumEventIdFromThisSubsection =
                        sortedEventIds.last
                      val annulmentsForExtraEventIdsNotCorrectedInThisSubsection =
                        Stream(
                          (1 + maximumEventIdFromThisSubsection) to maximumEventIdFromPreviousSubsection map ((None: Option[
                            (Unbounded[Instant], Event)]) -> _))
                      val asOfForAnnulments = asOfs.head
                      try {
                        recordEventsInWorld(
                          annulmentsForExtraEventIdsNotCorrectedInThisSubsection,
                          List(asOfForAnnulments),
                          world)
                        recordEventsInWorld(bigShuffledHistoryOverLotsOfThings,
                          asOfs,
                          world)
                      } catch {
                        case _: RuntimeException =>
                          // The assumption is that our brute-force rewriting of history made what would be
                          // an inconsistent revision as an intermediate step. In this case, annul the history
                          // entirely from the previous subsection and rewrite from a clean slate. We assume
                          // that if there was an inconsistency between the previous history that wasn't yet
                          // fully corrected and whatever attempted revision that caused the exception
                          // to be thrown, then there is obviously at least one previous revision to steal an asOf
                          // from.
                          // NOTE: annulling everything and rewriting is a bit prolix compared with flattening
                          // the history and correcting in a single grand slam revision without going through any
                          // intermediate steps. However, the prolix way happened to expose a bug in the tests not observed
                          // when doing a grand slam revision, so we'll stick with it for now.
                          assert(World.initialRevision != world.nextRevision)
                          val asOfForAllCorrections = asOfs.last

                          val annulmentsGalore = Stream(
                            (0 to (maximumEventIdFromPreviousSubsection max maximumEventIdFromThisSubsection)) map ((None: Option[
                              (Unbounded[Instant], Event)]) -> _))

                          recordEventsInWorld(annulmentsGalore,
                            List(asOfForAllCorrections),
                            world)
                          recordEventsInWorld(
                            bigShuffledHistoryOverLotsOfThings,
                            List.fill(asOfs.length)(asOfForAllCorrections),
                            world)
                      }

                      Some(
                        (world.nextRevision   -> recordingsGroupedById,
                          remainingSubsections -> maximumEventIdFromThisSubsection))
                    case _ => None
                  }

                val checks = (for {
                  (revision, recordingsGroupedById) <- listOfRevisionsToCheckAtAndRecordingsGroupedById
                } yield {
                  val scope = world.scopeFor(queryWhen, revision)

                  val checks = for {
                    RecordingsNoLaterThan(
                    historyId,
                    historiesFrom,
                    pertinentRecordings,
                    _,
                    _) <- recordingsGroupedById flatMap (_.thePartNoLaterThan(
                      queryWhen))
                    Seq(history) = historiesFrom(scope)
                  } yield
                    (historyId, history.datums, pertinentRecordings.map(_._1))

                  Prop.all(checks.map {
                    case (historyId, actualHistory, expectedHistory) =>
                      ((actualHistory.length == expectedHistory.length) :| s"For ${historyId}, expected to see: ${expectedHistory.length} datums, but got: ${actualHistory.length} - actual history: ${actualHistory.toList}, expected history: ${expectedHistory}.") &&
                        Prop.all(
                          (actualHistory zip expectedHistory zipWithIndex) map {
                            case ((actual, expected), step) =>
                              (actual == expected) :| s"For ${historyId}, @step ${step}, ${actual} was expected to be: ${expected}"
                          }: _*)
                  }: _*)
                }).force

                if (checks.nonEmpty)
                  Prop.all(checks: _*)
                else Prop.undecided
            }
        }
      )
    }

    it should "allow an entire history to be completely annulled and then rewritten at the same asOf" in {
      val testCaseGenerator = for {
        worldResource <- worldResourceGenerator
        recordingsGroupedById <- recordingsGroupedByIdGenerator(
          forbidAnnihilations = false)
        seed <- seedGenerator
        random = new Random(seed)
        bigShuffledHistoryOverLotsOfThings = random.splitIntoNonEmptyPieces(
          shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
            random,
            recordingsGroupedById).zipWithIndex)
        allEventIds = bigShuffledHistoryOverLotsOfThings flatMap (_ map (_._2))
        annulmentsGalore = Stream(
          allEventIds map ((None: Option[(Unbounded[Instant], Event)]) -> _))
        historyLength    = bigShuffledHistoryOverLotsOfThings.length
        annulmentsLength = annulmentsGalore.length
        asOfs     <- Gen.listOfN(historyLength, instantGenerator) map (_.sorted)
        queryWhen <- unboundedInstantGenerator
      } yield
        (worldResource,
          recordingsGroupedById,
          bigShuffledHistoryOverLotsOfThings,
          annulmentsGalore,
          asOfs,
          queryWhen)
      check(Prop.forAllNoShrink(testCaseGenerator) {
        case (worldResource,
        recordingsGroupedById,
        bigShuffledHistoryOverLotsOfThings,
        annulmentsGalore,
        asOfs,
        queryWhen) =>
          worldResource acquireAndGet {
            world =>
              // Define a history the first time around...

              recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                asOfs,
                world)

              val scopeForFirstHistory =
                world.scopeFor(queryWhen, world.nextRevision)

              val firstHistory =
                historyFrom(world, recordingsGroupedById)(scopeForFirstHistory)

              // Annul that history completely...

              val asOfForCorrections = asOfs.last

              recordEventsInWorld(annulmentsGalore,
                List(asOfForCorrections),
                world)

              val scopeAfterAnnulments =
                world.scopeFor(queryWhen, world.nextRevision)

              val historyAfterAnnulments =
                historyFrom(world, recordingsGroupedById)(scopeAfterAnnulments)

              // ...and then recreate what should be the same history.

              recordEventsInWorld(
                liftRecordings(bigShuffledHistoryOverLotsOfThings),
                List.fill(asOfs.length)(asOfForCorrections),
                world)

              val scopeForSecondHistory =
                world.scopeFor(queryWhen, world.nextRevision)

              val secondHistory =
                historyFrom(world, recordingsGroupedById)(scopeForSecondHistory)

              (historyAfterAnnulments.isEmpty :| s"${historyAfterAnnulments}.isEmpty") &&
                ((firstHistory == secondHistory) :| s"firstHistory === ${secondHistory}")
          }
      })
    }
  }
}

class WorldSpecUsingWorldReferenceImplementation
  extends WorldBehaviours
    with WorldReferenceImplementationResource {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(maxSize = 40, minSuccessful = 100)

  "A world with no history (using the world reference implementation)" should behave like worldWithNoHistoryBehaviour

  "A world with history added in order of increasing event time (using the world reference implementation)" should behave like worldWithHistoryAddedInOrderOfIncreasingEventTimeBehaviour

  "A world (using the world reference implementation)" should behave like worldBehaviour

  "A world with events that have since been corrected (using the world reference implementation)" should behave like worldWithEventsThatHaveSinceBeenCorrectedBehaviour
}

class WorldSpecUsingWorldEfficientInMemoryImplementation
  extends WorldBehaviours
    with WorldEfficientInMemoryImplementationResource {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(maxSize = 40, minSuccessful = 100)

  "A world with no history (using the world efficient in-memory implementation)" should behave like worldWithNoHistoryBehaviour

  "A world with history added in order of increasing event time (using the world efficient in-memory implementation)" should behave like worldWithHistoryAddedInOrderOfIncreasingEventTimeBehaviour

  "A world (using the world efficient in-memory implementation)" should behave like worldBehaviour

  "A world with events that have since been corrected (using the world efficient in-memory implementation)" should behave like worldWithEventsThatHaveSinceBeenCorrectedBehaviour
}

abstract class HistoryWhoseIdWontSerialize extends History {
  type Id = WontSerializeId

  var property: String = ""
}

case class BadSerializationException() extends RuntimeException {}

case class WontSerializeId(var id: Int) extends KryoSerializable {
  override def write(kryo: Kryo, output: Output): Unit =
    throw BadSerializationException()

  override def read(kryo: Kryo, input: Input): Unit = {
    id = kryo.readObject(input, classOf[Int])
  }
}

abstract class HistoryWhoseIdWontDeserialize extends History {
  type Id = WontDeserializeId

  var property: Boolean = false
}

case class BadDeserializationException() extends RuntimeException {}

case class WontDeserializeId(var id: String) extends KryoSerializable {
  override def write(kryo: Kryo, output: Output): Unit =
    kryo.writeObject(output, id)

  override def read(kryo: Kryo, input: Input): Unit =
    throw BadDeserializationException()
}

class WorldSpecUsingWorldRedisBasedImplementation
  extends WorldBehaviours
    with WorldRedisBasedImplementationResource {
  val redisServerPort = 6454

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(maxSize = 15)

  "A world with no history (using the world Redis-based implementation)" should behave like worldWithNoHistoryBehaviour

  "A world with history added in order of increasing event time (using the world Redis-based implementation)" should behave like worldWithHistoryAddedInOrderOfIncreasingEventTimeBehaviour

  "A world (using the world Redis-based implementation)" should behave like worldBehaviour

  "A world with events that have since been corrected (using the world Redis-based implementation)" should behave like worldWithEventsThatHaveSinceBeenCorrectedBehaviour

  implicit class ThrowableEnhancement(throwable: Throwable) {
    def rootCause = rootCauseOf(throwable)

    private def rootCauseOf(throwable: Throwable): Throwable = {
      val cause = Option(throwable.getCause)
      cause.fold(throwable)(rootCauseOf(_))
    }
  }

  "A world" should "be usable even if (de)serialization fails" in {
    val testCaseGenerator = for {
      worldResource <- worldResourceGenerator
      asOf          <- instantGenerator
      queryWhen     <- unboundedInstantGenerator
    } yield (worldResource, asOf, queryWhen)
    check(Prop.forAllNoShrink(testCaseGenerator) {
      case (worldResource, asOf, queryWhen) =>
        worldResource acquireAndGet {
          world =>
            val itemOneId = WontSerializeId(1)

            val exceptionDueToFailedSerialization = intercept[KryoException] {
              world.revise(
                Map(
                  100 -> Some(Change.forOneItem[HistoryWhoseIdWontSerialize](
                    NegativeInfinity[Instant]())(itemOneId,
                    (_.property = "Fred")))),
                asOf)
            } rootCause

            val serializationFailedInExpectedManner = (exceptionDueToFailedSerialization == BadSerializationException()) :| s"Expected an instance of 'BadSerializationException', but got a '$exceptionDueToFailedSerialization' instead."

            val firstRevisionAttemptFailed = (world.nextRevision == World.initialRevision) :| s"The first revision attempt should have failed due to serialization throwing an exception."

            val itemTwoId = 28

            world.revise(
              Map(
                300 -> Some(Change.forOneItem[BarHistory](
                  NegativeInfinity[Instant]())(itemTwoId, (_.property1 = 4)))),
              asOf)

            val exceptionDueToFailedSecondSerialization =
              intercept[KryoException] {
                world.revise(
                  Map(
                    400 -> Some(Change.forOneItem[HistoryWhoseIdWontSerialize](
                      NegativeInfinity[Instant]())(itemOneId,
                      (_.property = "Alfred")))),
                  asOf)
              } rootCause

            val secondSerializationFailedInExpectedManner = (exceptionDueToFailedSecondSerialization == BadSerializationException()) :| s"Expected an instance of 'BadSerializationException', but got a '$exceptionDueToFailedSerialization' instead."

            val secondRevisionAttemptFailed = (world.nextRevision == 1 + World.initialRevision) :| s"The second revision attempt should have failed due to serialization throwing an exception."

            val queryOk = (world
              .scopeFor(queryWhen, world.nextRevision)
              .render(Bitemporal.withId[BarHistory](itemTwoId))
              .head
              .datums == Seq(4)) :| "Expected to see effects of the successful revision."

            val itemThreeId = WontDeserializeId("Elma")

            world.revise(
              Map(
                200 -> Some(
                  Change.forOneItem[HistoryWhoseIdWontDeserialize](
                    NegativeInfinity[Instant]())(itemThreeId,
                    (_.property = true)))),
              asOf)

            val exceptionDueToFailedDeserialization =
              intercept[KryoException] {
                val scope = world.scopeFor(queryWhen, world.nextRevision)
                scope.render(Bitemporal.wildcard[History]())
              } rootCause

            val deserializationFailedInExpectedManner = (exceptionDueToFailedDeserialization == BadDeserializationException()) :| s"Expected an instance of 'BadDeserializationException', but got a '$exceptionDueToFailedDeserialization' instead."

            serializationFailedInExpectedManner && firstRevisionAttemptFailed &&
              deserializationFailedInExpectedManner &&
              secondSerializationFailedInExpectedManner && secondRevisionAttemptFailed &&
              queryOk
        }
    })
  }
}

class AllTheWorlds
  extends FlatSpec
    with Matchers
    with Checkers
    with WorldSpecSupport
    with WorldRedisBasedImplementationResource {
  val redisServerPort = 6455

  object worldReferenceImplementationResource
    extends WorldReferenceImplementationResource

  object worldEfficientInMemoryImplementationResource
    extends WorldEfficientInMemoryImplementationResource

  "all the world implementations" should "agree" in {
    val testCaseGenerator = for {
      worldReferenceImplementationResource         <- worldReferenceImplementationResource.worldResourceGenerator
      worldEfficientInMemoryImplementationResource <- worldEfficientInMemoryImplementationResource.worldResourceGenerator
      worldRedisBasedImplementationResource        <- worldResourceGenerator
      recordingsGroupedById <- recordingsGroupedByIdGenerator(
        forbidAnnihilations = false)
      obsoleteRecordingsGroupedById <- nonConflictingRecordingsGroupedByIdGenerator
      seed                          <- seedGenerator
      random = new Random(seed)
      shuffledRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
        random,
        recordingsGroupedById)
      shuffledObsoleteRecordings = shuffleRecordingsPreservingRelativeOrderOfEventsAtTheSameWhen(
        random,
        obsoleteRecordingsGroupedById)
      bigShuffledHistoryOverLotsOfThings = intersperseObsoleteEvents(
        random,
        shuffledRecordings,
        shuffledObsoleteRecordings)
      asOfs <- Gen.listOfN(bigShuffledHistoryOverLotsOfThings.length,
        instantGenerator) map (_.sorted)
      queryWhen <- unboundedInstantGenerator
    } yield
      (worldReferenceImplementationResource,
        worldEfficientInMemoryImplementationResource,
        worldRedisBasedImplementationResource,
        recordingsGroupedById,
        bigShuffledHistoryOverLotsOfThings,
        asOfs,
        queryWhen)
    check(Prop.forAllNoShrink(testCaseGenerator) {
      case (worldReferenceImplementationResource,
      worldEfficientInMemoryImplementationResource,
      worldRedisBasedImplementationResource,
      recordingsGroupedById,
      bigShuffledHistoryOverLotsOfThings,
      asOfs,
      queryWhen) =>
        def resultsFrom(world: World): immutable.Seq[(Any, Seq[Any])] = {
          recordEventsInWorld(bigShuffledHistoryOverLotsOfThings, asOfs, world)

          val scope = world.scopeFor(queryWhen, world.nextRevision)

          for {
            RecordingsNoLaterThan(historyId, historiesFrom, _, _, _) <- recordingsGroupedById flatMap (_.thePartNoLaterThan(
              queryWhen))
            Seq(history) = historiesFrom(scope)
          } yield (historyId, history.datums)
        }

        val checks = for {
          worldReferenceImplementation         <- worldReferenceImplementationResource
          worldEfficientInMemoryImplementation <- worldEfficientInMemoryImplementationResource
          worldRedisBasedImplementation        <- worldRedisBasedImplementationResource
        } yield {
          val worldReferenceImplementationResults =
            resultsFrom(worldReferenceImplementation)
          val worldEfficientInMemoryImplementationResults =
            resultsFrom(worldEfficientInMemoryImplementation)
          val redisBasedImplementationResults =
            resultsFrom(worldRedisBasedImplementation)

          ((worldReferenceImplementationResults == worldEfficientInMemoryImplementationResults) :| s"Should have agreement between reference implementation and efficient in-memory implementation.") &&
            ((worldEfficientInMemoryImplementationResults == redisBasedImplementationResults) :| s"Should have agreement between efficient in-memory implementation and Redis based implementation.") &&
            ((redisBasedImplementationResults == worldReferenceImplementationResults) :| s"Should have agreement between Redis based implementation and reference implementation.")
        }

        checks acquireAndGet identity
    })
  }
}
