import sbt._

class BenchmarkSuite(info: ProjectInfo) extends DefaultProject(info)
{
  override def mainClass = Some("org.scalalites.benchmark.SampleBenchmark")
}
