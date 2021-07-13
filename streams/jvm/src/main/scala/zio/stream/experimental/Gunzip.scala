package zio.stream.experimental

import zio.stream.compression.{CompressionException, Gunzipper}
import zio.{Chunk, ZIO, ZManaged}

object Gunzip {
  def makeGunzipper[Done](
    bufferSize: Int = 64 * 1024
  ): ZChannel[Any, CompressionException, Chunk[Byte], Done, CompressionException, Chunk[Byte], Done] =
    ZChannel.managed(
      ZManaged.make(
        Gunzipper.make(bufferSize)
      )(gunzipper => ZIO.effectTotal(gunzipper.close()))
    ) {
      case gunzipper => {

        lazy val loop: ZChannel[Any, CompressionException, Chunk[Byte], Done, CompressionException, Chunk[Byte], Done] =
          ZChannel.readWithCause(
            chunk =>
              ZChannel.fromEffect {
                gunzipper.onChunk(chunk)
              }.flatMap(chunk => ZChannel.write(chunk) *> loop),
            ZChannel.halt(_),
            done =>
              ZChannel.fromEffect {
                gunzipper.onNone
              }.flatMap(chunk => ZChannel.write(chunk).as(done))
          )

        loop
      }
    }

}
