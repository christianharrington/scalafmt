package org.scalafmt.sysops

import org.scalafmt.CompatCollections.JavaConverters._

import java.io.File
import java.nio.file.{Files, Path}
import java.util.concurrent.{
  Executors, SynchronousQueue, ThreadPoolExecutor, TimeUnit,
}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.util.{Failure, Success, Try}

private[scalafmt] object PlatformRunOps {

  implicit def executionContext: ExecutionContext = ExecutionContext.global

  private val ncores = Runtime.getRuntime.availableProcessors()

  // creates non-daemon threads
  val inputExecutionContext: ExecutionContextExecutorService = ExecutionContext
    .fromExecutorService(Executors.newFixedThreadPool(ncores))

  lazy val formatExecutionContext: ExecutionContext = {
    val queue = new SynchronousQueue[Runnable]() {
      override def offer(e: Runnable): Boolean = { put(e); true } // blocks
    }
    ExecutionContext.fromExecutorService(
      new ThreadPoolExecutor(ncores, ncores, 0L, TimeUnit.MILLISECONDS, queue),
    )
  }

  val outputExecutionContext: ExecutionContextExecutorService = ExecutionContext
    .fromExecutorService(Executors.newFixedThreadPool(ncores))

  implicit def parasiticExecutionContext: ExecutionContext =
    GranularDialectAsyncOps.parasiticExecutionContext

  def runArgv(cmd: Seq[String], cwd: Option[Path]): Try[Seq[String]] = {
    // Use temp files to avoid pipe buffer deadlock on large outputs with scala-native
    // Can be removed once scala-native is upgraded to 0.5.10
    // See https://github.com/scalameta/scalafmt/issues/5165
    val stdoutFile = File.createTempFile("scalafmt-stdout", ".tmp")
    val stderrFile = File.createTempFile("scalafmt-stderr", ".tmp")
    def failed(err: String)(e: Throwable) = {
      val msg = cmd
        .addString(new StringBuilder(), "Failed to run '", " ", "'. Error: ")
        .append(err).append('\n')
      Failure(new IllegalStateException(msg.toString(), e))
    }
    try {
      val pb = new ProcessBuilder(cmd.asJava)
      cwd.foreach(p => pb.directory(p.toFile))
      pb.redirectOutput(stdoutFile)
      pb.redirectError(stderrFile)

      val process = pb.start()
      val exit = process.waitFor()

      val out = Files.readAllLines(stdoutFile.toPath).asScala.toSeq
      val err = Files.readAllLines(stderrFile.toPath).asScala.mkString("\n")

      if (exit != 0) failed(err)(new RuntimeException("exit code " + exit))
      else Success(out)
    } catch { case e: Throwable => failed("")(e) }
    finally {
      stdoutFile.delete()
      stderrFile.delete()
    }
  }

  def exit(code: Int): Nothing = sys.exit(code)

}
