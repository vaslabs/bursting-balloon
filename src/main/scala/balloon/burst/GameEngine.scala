package balloon.burst

import balloon.burst.protocol._

object GameEngine {

  case class StreamState(commands: LazyList[Command], balloons: List[InflateableBalloon], state: State)
  object StreamState {
    def init(commands: LazyList[Command], balloons: List[InflateableBalloon]): StreamState =
      StreamState(commands, balloons, WaitingForBalloon(List.empty))

  }

  def balloonStream(commands: LazyList[Command]): LazyList[Event] = {
    val startCommand = commands.take(1).headOption.collect {
      case s: Start =>
        s
    }
    startCommand.fold[LazyList[Event]](LazyList(End(0))) { start =>
      LazyList.unfold[Event, StreamState](StreamState.init(commands, start.balloons)) {
        case StreamState(commands, balloons, w @ WaitingForBalloon(stash)) =>
          val remainingBalloons = balloons.drop(1)
          balloons.headOption match {
            case Some(nextBalloon) =>
              for {
              nextCommand <- commands.headOption
              dropHeadCommands = commands.drop (1)
              (state, event) = executeCommand (nextCommand, w.acceptNewBalloon(nextBalloon))
              } yield event -> StreamState (dropHeadCommands, remainingBalloons, state)
            case None =>
              Some(w.end -> StreamState(LazyList.empty, List.empty, Finish (stash)))
          }

        case StreamState(commands, balloons, s: StateWithBalloon) =>
          val dropHeadCommand = commands.drop(1)
          for {
            nextCommand <- commands.headOption
            (state, event) = executeCommand(nextCommand, s)
          } yield event -> StreamState(dropHeadCommand, balloons, state)
        case StreamState(_, _, Finish(_)) =>
          None
      }
    }
  }

  private def executeCommand(cmd: Command, state: StateWithBalloon): (State, Event) =
    cmd match {
      case Inflate =>
        state.inflate
      case Bank =>
        state.bank
      case _ =>
        state -> Inflated
    }
}
