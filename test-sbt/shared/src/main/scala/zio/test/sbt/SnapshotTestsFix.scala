//package zio.test.sbt
//import _root_.sbt._
//import Keys._
//
//object SnapshotTestsFix extends AutoPlugin {
//
//  override val trigger: PluginTrigger = noTrigger
//  override val requires: Plugins = plugins.JvmPlugin
//
//  object autoImport {
//    val snapshotTestsFix = taskKey[Unit]("Fixes the snapshot tests")
//  }
//
//  import autoImport._
//
//  override lazy val projectSettings: Seq[Setting[_]] =Seq(
//    snapshotTestsFix := snapshotTestsFixTask.value
//  )
//
//  private def snapshotTestsFixTask =  Def.task {
////    val log = sLog.value
////    lazy val zip = new File(targetZipDir.value,
////      sourceZipDir.value.getName + ".zip")
////    log.info("Zipping file...")
////    IO.zip(Path.allSubpaths(sourceZipDir.value), zip)
////    zip
//    println("snapshotTestsFixTask")
//  }
//}