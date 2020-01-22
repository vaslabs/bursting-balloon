package balloon.burst

import balloon.burst.codec.Codec
import balloon.burst.protocol.Command

import scala.io.Source

object Main extends App {

  val commands: LazyList[Command] = LazyList.from(
    Source.fromInputStream(System.in).getLines().iterator
  ).map(Codec.commands.fromString).collect {
    case Right(c) =>
      c
  }

  GameEngine.balloonStream(commands).map (
      Codec.events.show
  ).foreach(_.foreach(println))

}
