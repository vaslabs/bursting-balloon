package balloon.burst.codec

import balloon.burst.protocol.{Bank, Burst, Command, End, Event, Inflate, InflateableBalloon, Start}

import scala.util.Try

object Codec {

  object commands {

    def fromString(value: String): Either[String, Command] =
      commandParser(value)

    private final val commandParser: Function[String, Either[String, Command]] = {
      case "INFLATE" =>
        Right(Inflate)
      case "BANK" =>
        Right(Bank)
      case other =>
        Try[Command](
          Start(
            other
              .split(" ")
              .map(_.toInt)
              .map(InflateableBalloon(_, 0))
              .toList
          )
        ).toEither.left.map(_ => s"Unrecognised command $other")
    }

  }

  object events {
    def show(event: Event): Option[String] =
      event match {
        case Burst =>
          Some("BURST")
        case End(score) =>
          Some(s"SCORE: $score")
        case _ =>
          None
      }
  }

}
