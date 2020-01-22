package balloon.burst

object protocol {


  sealed trait Command
  case class Start(balloons: List[InflateableBalloon]) extends Command
  case object Inflate extends Command
  case object Bank extends Command

  sealed trait Event
  case object Inflated extends Event
  case object Burst extends Event
  case object Store extends Event
  case class End(score: Long) extends Event

  sealed trait Balloon {
    def score: Int
  }

  case class InflateableBalloon(limit: Int, inflateLevel: Int) extends Balloon {
    override def score: Int = inflateLevel

    def inflate: Balloon =
      if (inflateLevel == limit)
        BurstBalloon
      else
        InflateableBalloon(limit, inflateLevel + 1)
  }

  case object BurstBalloon extends Balloon {
    override def score: Int = 0
  }


  sealed trait State

  case class StateWithBalloon(currentBalloon: InflateableBalloon, stash: List[Balloon]) extends State {
    def inflate: (State, Event) = currentBalloon.inflate match {
      case BurstBalloon =>
        WaitingForBalloon(stash :+ BurstBalloon) -> Burst
      case i: InflateableBalloon =>
        StateWithBalloon(i, stash) -> Inflated
    }

    def bank: (State, Event) = WaitingForBalloon(stash :+ currentBalloon) -> Store

  }

  case class WaitingForBalloon(stash: List[Balloon]) extends State {
    def acceptNewBalloon(inflateableBalloon: InflateableBalloon): StateWithBalloon =
      StateWithBalloon(inflateableBalloon, stash)
    def end: Event =
      End(stash.map(_.score.toLong).sum[Long])
  }

  case class Finish(stash: List[Balloon]) extends State

}
