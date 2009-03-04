/**
 * BenchmarkSuite - scalalites.org
 *
 *  Copyright (c) 2009 Rakuto Furutani (rakuto@gmail.com)
 *  All rights reserved.
 *
 *  Permission to use, copy, modify, and distribute this software in source
 *  or binary form for any purpose with or without fee is hereby granted,
 *  provided that the following conditions are met:
 *
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
 */

package org.scalalites.benchmark

import java.io.{OutputStream, PrintStream, FileOutputStream}
import java.lang.System
import scala.collection.mutable.{Map, HashMap, ListBuffer, Set}

/**
 * Benchmark object provides simple benchmark suite, scala's standard benchmark library
 * was not useful for me, this class provides to way to measure execute time of code block.
 * 
 * Getting started:
 *      import org.scalalites.benchmark.Benchmark
 *
 *      Benchmark.run("Benchmark of ack:") { b =>
 *        def ack(x: Int, y: Int): Int = x match {
 *          case 0 => y + 1
 *          case _ => y match { case 0 => ack(x - 1, 1); case _ => ack(x - 1, ack(x, y - 1))}
 *        }
 *
 *        // run over the test named "ack(3, 2) x 50" 50 times
 *        b.report("ack(3, 4) x 10", 10)  { ack(3, 4) }
 *
 *        // If you don't pass second argument run benchmark once.
 *        b.report("ack(3, 2)")  { ack(3, 2) }
 *      }
 *      
 *      // Output
 *      // Benchmark of ack:
 *      //  ack(3, 4) x 10 : 4.730000 ms (avg. 0.473000 ms)
 *      //  ack(3, 2) x 1  : 0.029000 ms
 */
object Benchmark
{
  var ostream = System.out

  /**
   * Repoterter class used by block arguments of Benchmark.run method.
   * This class has one public method, Repoterter#run.
   */
  sealed class Reporter
  {
    // save result of benchmark test
    var result: ListBuffer[(String, ListBuffer[Double])] = new ListBuffer()

    // public API
    def report(mult: Int)(block: => Unit): Unit = report("", mult)(block)
    def report(label: String)(block: => Unit): Unit = report(label, 1)(block)
    def report(label: String, mult: Int)(block: => Unit): Unit = {
      var v: Any = null
      val start = System.currentTimeMillis()
      for(i <- 0 to mult) { v = block } // XXX: disable effect of optimization
      val stop = System.currentTimeMillis()
      val lb = new ListBuffer[Double]()
      lb += (stop - start)
      result += (label, lb)
      if(mult > 1) result.last._2 += (result.last._2(0) / mult)
    }
  }

  // run the benchmark test
  def run(label: String)(action: Reporter => Unit) = {
    val reporter = new Reporter
    action(reporter)
    var result = format(label, reporter)
    print0(ostream, result)
  }

  private def print0(ostream: PrintStream, result: String) = ostream.println(result)
  private def print0(ostream: FileOutputStream, result: String) = ostream.write(result.toArray.map(_.toByte))
  private def format(label: String, reporter: Reporter) = {
    reporter.result.map { r => 
      var msec = String.format("%05f", (r._2(0) / 1000): java.lang.Double)
      var detail  = "  " + r._1 + " : " + msec + " ms"
      if(r._2.length > 1)  {
        msec = String.format("%05f", (r._2(1) / 1000): java.lang.Double)
        detail += msec.toString.mkString(" (avg. ", "", " ms)")
      }
      detail
    }.mkString(label + "\n", "\n", "")
  }
}

object SampleBenchmark
{
    def main(args: Array[String]) = {
      def fib(n: Int): Int = if(n == 0) 1 else fib(n - 1)
      def ack(x: Int, y: Int): Int = x match {
        case 0 => y + 1
        case _ => y match { case 0 => ack(x - 1, 1); case _ => ack(x - 1, ack(x, y - 1))}
      }

      Benchmark.run("parse Scala programs") { b =>
        b.report("ack(3, 2) x 1  ") { ack(3, 2) }
        b.report("ack(3, 4) x 10", 10) { ack(3, 10) }
        None
      }
    }
}
