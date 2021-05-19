package zio.test.sbt

import zio.test.sbt.LayerCache.LayerCacheMap
import zio._
import zio.test.AbstractRunnableSpec

case class LayerCache[R](
  private val layerMap: Ref[LayerCacheMap[R]],
  private val available: Promise[Nothing, Unit],
  private val release0: Promise[Nothing, Unit]
) {

  def cacheLayers(
    layers: Iterable[URLayer[R, _]],
    env: ULayer[R]
  ): UIO[Unit] = {
    val layersToBeCached: Set[URLayer[R, _]] = layers.toSet
    ZIO.foreachPar_(layersToBeCached) { layer =>
      // UIO(println(s"add to cache: $layer")) *>
      (env >>> layer).build.use { memoizedLayer =>
        for {
          cache <- layerMap.updateAndGet(
                     _ + (layer -> memoizedLayer)
                   )
          _ <- ZIO.when(cache.size == layersToBeCached.size)(
                 available.succeed(())
               )
          _ <- release0.await *> layerMap.update(_ - layer)
        } yield ()
      }.forkDaemon
    }
  }

  val awaitAvailable: UIO[Unit] = available.await

  val release: UIO[Unit] = release0.succeed(()).unit

  def getLayer[ROut](layer: URLayer[R, ROut]): UIO[ROut] =
    // UIO(println(s"get from cache: ${layer}")) *>
    layerMap.get.map(_.apply(layer).asInstanceOf[ROut])

  def debug =
    layerMap.get.flatMap { cache =>
      UIO(
        println(
          "cache:\n" + cache.map { case (k, v) =>
            s"  $k -> @${System.identityHashCode(v).toHexString}"
          }.mkString("\n")
        )
      )
    }
}

object LayerCache {

  type LayerCacheMap[R] = Map[URLayer[R, _], _]

  def make[R]: UIO[LayerCache[R]] =
    for {
      layerCache <- Ref.make[LayerCacheMap[R]](Map.empty)
      available  <- Promise.make[Nothing, Unit]
      release    <- Promise.make[Nothing, Unit]
    } yield LayerCache(layerCache, available, release)
}

case class CustomSpecLayerCache(
  private val layerCache: LayerCache[ZEnv]
) {
  val awaitAvailable: UIO[Unit] = layerCache.awaitAvailable
  val debug: UIO[Unit]          = layerCache.debug
  val release: UIO[Unit]        = layerCache.release

  def cacheLayers(specs: Iterable[AbstractRunnableSpec]): UIO[Unit] =
    layerCache.cacheLayers(
      specs.map(spec => spec.sharedLayer),
      ZEnv.live
    )

  def getEnvironment[R <: Has[_]](layer: URLayer[ZEnv, R]): UIO[R] =
    layerCache.getLayer[R](layer)
}

object CustomSpecLayerCache {
  def make: UIO[CustomSpecLayerCache] =
    LayerCache.make[ZEnv].map(CustomSpecLayerCache.apply)
}
