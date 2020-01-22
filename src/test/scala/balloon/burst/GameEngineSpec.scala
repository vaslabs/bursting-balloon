package balloon.burst

import balloon.burst.protocol._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

class GameEngineSpec extends Properties("GameEngine"){
  import GameEngineSpec._

  property("wineverything") = forAll(winPointsForEveryBalloonStrategy){ game =>
    val allEvents =
      GameEngine.balloonStream(
        LazyList.from(game.gameStream)).foldLeft(List.empty[Event])(_ :+ _)

    allEvents.count(_ == Burst) == 0 &&
    allEvents.takeRight(1) == List(End(game.expectedScore.toLong))
  }

  property("burstall") = forAll(loseAllStrategy) {
    game =>
      val allEvents = GameEngine.balloonStream(
        LazyList.from(game.gameStream)).foldLeft(List.empty[Event])(_ :+ _)

      allEvents.count(_ == Burst) == game.balloons.size &&
        allEvents.takeRight(1) == List(End(0))
  }

  property("burstsome") = forAll(winSomeStrategy) {
    game =>
      val allEvents = GameEngine.balloonStream(
        LazyList.from(game.gameStream)
      ).foldLeft(List.empty[Event])(_ :+ _)

      val endEvent = allEvents.takeRight(1)
      val expectedEndEvent = List(End(game.expectedScore.toLong))

      endEvent == expectedEndEvent
  }

}

object GameEngineSpec {
  case class Game(balloons: List[InflateableBalloon], strategy: List[List[Command]]) {
    val expectedScore =
      strategy.flatten.count(_ == Inflate)

    val gameStream =
      Start(balloons) +: strategy.flatten
  }

  case class WinSomeGame(
          balloons: List[InflateableBalloon],
          wins: List[Int], strategy: List[List[Command]]
  ) {
    val expectedScore =
      wins.sum
    val gameStream =
      Start(balloons) +: strategy.flatten

  }

  def winPointsForSure(balloon: InflateableBalloon): List[Command] = (
    for {
      inflateNo <- Gen.choose(0, balloon.limit)
      inflateCommands <- Gen.listOfN(inflateNo, Gen.const(Inflate))
    } yield inflateCommands :+ Bank
  ).sample.get

  def loseEverything(balloon: InflateableBalloon): List[Command] =
    Gen.listOfN(balloon.limit + 1, Inflate).sample.get

  def maybeWin(balloon: InflateableBalloon): (Int, List[Command]) =
    Gen.oneOf(
      Gen.choose(0, balloon.limit)
        .flatMap(Gen.listOfN(_, Gen.const(Inflate)))
        .map(l => l.size -> (l :+ Bank)),
      Gen.listOfN[Command](balloon.limit + 1, Gen.const(Inflate)).map(0 -> _)
    ).sample.get

  val winPointsForEveryBalloonStrategy: Gen[Game] = for {
    balloonLimits <-
      Gen.listOf(Gen.choose(1, Byte.MaxValue).map(InflateableBalloon(_, 0)))
    commands = balloonLimits.map(winPointsForSure)
  } yield Game(balloonLimits, commands)


  val loseAllStrategy: Gen[Game] = for {
    balloonLimits <- Gen.listOf(
      Gen.choose(1, Byte.MaxValue).map(InflateableBalloon(_, 0))
    )
    commands = balloonLimits.map(loseEverything)
  } yield Game(balloonLimits, commands)

  val winSomeStrategy: Gen[WinSomeGame] = for {
    balloonLimits <- Gen.listOf(
      Gen.choose(1, Byte.MaxValue).map(InflateableBalloon(_, 0))
    )
    commandsWithExpectedPoints = balloonLimits.map(maybeWin)
    expectedPoints = commandsWithExpectedPoints.map(_._1)
    strategy = commandsWithExpectedPoints.map(_._2)
  } yield WinSomeGame(balloonLimits, expectedPoints, strategy)
}
