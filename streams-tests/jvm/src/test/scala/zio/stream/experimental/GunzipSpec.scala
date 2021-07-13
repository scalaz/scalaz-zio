package zio.stream.experimental

import zio.stream.compression.TestData.{`1K`, jdkGzip, longText, otherShortText, shortText}
import zio.test.Assertion._
import zio.test._
import zio.{Chunk, ZIO}

import java.nio.charset.StandardCharsets
import java.util.zip.CRC32

object GunzipSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[Environment, Failure] =
    suite("Gunzip")(
      testM("short stream")(
        assertM(
          (jdkGzippedStream(shortText).channel >>> Gunzip.makeGunzipper(64)).runCollect.map(_._1.flatten)
        )(
          equalTo(Chunk.fromArray(shortText))
        )
      ),
      testM("stream of two gzipped inputs as a single chunk")(
        assertM(
          ((jdkGzippedStream(shortText) ++ jdkGzippedStream(otherShortText))
            .chunkN(500)
            .channel >>> Gunzip.makeGunzipper(64)).runCollect.map(_._1.flatten)
        )(equalTo(Chunk.fromArray(shortText) ++ Chunk.fromArray(otherShortText)))
      ),
      testM("long input")(
        assertM(
          (jdkGzippedStream(longText).channel >>> Gunzip.makeGunzipper(64)).runCollect.map(_._1.flatten)
        )(equalTo(Chunk.fromArray(longText)))
      ),
      testM("long input, no SYNC_FLUSH")(
        assertM(
          (jdkGzippedStream(longText, false).channel >>> Gunzip.makeGunzipper(64)).runCollect.map(_._1.flatten)
        )(equalTo(Chunk.fromArray(longText)))
      ),
      testM("long input, buffer smaller than chunks")(
        assertM(
          (jdkGzippedStream(longText).chunkN(500).channel >>> Gunzip.makeGunzipper(1)).runCollect.map(_._1.flatten)
        )(equalTo(Chunk.fromArray(longText)))
      ),
      testM("long input, chunks smaller then buffer")(
        assertM(
          (jdkGzippedStream(longText).chunkN(1).channel >>> Gunzip.makeGunzipper(500)).runCollect.map(_._1.flatten)
        )(equalTo(Chunk.fromArray(longText)))
      ),
      testM("fail early if header is corrupted")(
        assertM(
          (ZStream.fromIterable(1 to 10).map(_.toByte).channel >>> Gunzip.makeGunzipper()).runCollect.run
        )(fails(anything))
      ),
      testM("fail if input stream finished unexpected")(
        assertM(
          (jdkGzippedStream(longText).take(80).channel >>> Gunzip.makeGunzipper()).runCollect.run
        )(fails(anything))
      ),
      testM("no output on very incomplete stream is not OK")(
        assertM(
          (ZStream.fromIterable(1 to 5).map(_.toByte).channel >>> Gunzip.makeGunzipper()).runCollect.run
        )(fails(anything))
      ),
      testM("gunzip what JDK gzipped, nowrap")(
        checkM(Gen.listOfBounded(0, `1K`)(Gen.anyByte).zip(Gen.int(1, `1K`)).zip(Gen.int(1, `1K`))) {
          case ((chunk, n), bufferSize) =>
            assertM(for {
              deflated <- ZIO.effectTotal(jdkGzippedStream(chunk.toArray))
              out      <- (deflated.chunkN(n).channel >>> Gunzip.makeGunzipper(bufferSize)).runCollect.map(_._1.flatten)
            } yield out.toList)(equalTo(chunk))
        }
      ),
      testM("parses header with FEXTRA")(
        assertM(
          (headerWithExtra.channel >>> Gunzip.makeGunzipper(64)).runCollect.map(_._1.flatten)
        )(equalTo(Chunk.fromArray(shortText)))
      ),
      testM("parses header with FCOMMENT")(
        assertM(
          (headerWithComment.channel >>> Gunzip.makeGunzipper(64)).runCollect.map(_._1.flatten)
        )(equalTo(Chunk.fromArray(shortText)))
      ),
      testM("parses header with FNAME")(
        assertM(
          (headerWithFileName.channel >>> Gunzip.makeGunzipper(64)).runCollect.map(_._1.flatten)
        )(equalTo(Chunk.fromArray(shortText)))
      ),
      testM("parses header with CRC16")(
        assertM(
          (headerWithCrc.chunkN(1).channel >>> Gunzip.makeGunzipper(64)).runCollect.map(_._1.flatten)
        )(equalTo(Chunk.fromArray(shortText)))
      ),
      testM("parses header with CRC16, FNAME, FCOMMENT, FEXTRA")(
        assertM(
          (headerWithAll.channel >>> Gunzip.makeGunzipper(64)).runCollect.map(_._1.flatten)
        )(equalTo(Chunk.fromArray(shortText)))
      )
    )

  private def jdkGzippedStream(bytes: Array[Byte], syncFlush: Boolean = true): ZStream[Any, Nothing, Byte] =
    ZStream.fromIterable(jdkGzip(bytes, syncFlush))

  private def makeStreamWithCustomHeader(flag: Int, headerTail: Array[Byte]): ZStream[Any, Nothing, Byte] = {
    val headerHead = Array(31, 139, 8, flag, 0, 0, 0, 0, 0, 0).map(_.toByte)
    ZStream.fromIterable(headerHead ++ headerTail ++ jdkGzip(shortText).drop(10))
  }

  private val headerWithExtra: ZStream[Any, Nothing, Byte] =
    makeStreamWithCustomHeader(4, (Seq(13.toByte, 0.toByte) ++ Seq.fill(13)(42.toByte)).toArray)

  private val headerWithComment: ZStream[Any, Nothing, Byte] =
    makeStreamWithCustomHeader(16, "ZIO rocks!".getBytes(StandardCharsets.ISO_8859_1) ++ Array(0.toByte))

  private val headerWithFileName: ZStream[Any, Nothing, Byte] =
    makeStreamWithCustomHeader(8, "some-file-name.md".getBytes(StandardCharsets.ISO_8859_1) ++ Array(0.toByte))

  private val headerWithCrc: ZStream[Any, Nothing, Byte] = {
    val crcFlag     = 2
    val headerBytes = Array(31, 139, 8, crcFlag, 0, 0, 0, 0, 0, 0).map(_.toByte)
    val crc32       = new CRC32
    crc32.update(headerBytes)
    val crc16      = (crc32.getValue() & 0xffffL).toInt
    val crc16Byte1 = (crc16 & 0xff).toByte
    val crc16Byte2 = (crc16 >> 8).toByte
    val header     = headerBytes ++ Array(crc16Byte1, crc16Byte2)
    ZStream.fromIterable(header ++ jdkGzip(shortText).drop(10))
  }

  private val headerWithAll: ZStream[Any, Nothing, Byte] = {
    val flags         = 2 + 4 + 8 + 16
    val fixedHeader   = Array(31, 139, 8, flags, 0, 0, 0, 0, 0, 0).map(_.toByte)
    val extra         = (Seq(7.toByte, 0.toByte) ++ Seq.fill(7)(99.toByte)).toArray
    val fileName      = "win32.ini".getBytes(StandardCharsets.ISO_8859_1) ++ Array(0.toByte)
    val comment       = "the last test".getBytes(StandardCharsets.ISO_8859_1) ++ Array(0.toByte)
    val headerUpToCrc = fixedHeader ++ extra ++ fileName ++ comment
    val crc32         = new CRC32
    crc32.update(headerUpToCrc)
    val crc16      = (crc32.getValue() & 0xffffL).toInt
    val crc16Byte1 = (crc16 & 0xff).toByte
    val crc16Byte2 = (crc16 >> 8).toByte
    val header     = headerUpToCrc ++ Array(crc16Byte1, crc16Byte2)
    ZStream.fromIterable(header ++ jdkGzip(shortText).drop(10))
  }

}
