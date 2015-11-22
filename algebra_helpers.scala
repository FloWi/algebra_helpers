import scala.math._

def exps(numberOfExps: Int) = {
  (-numberOfExps).to(-1).map(e => pow(10, e)) ++
    (-numberOfExps).to(-1).map(e => -pow(10, e))
}

def lim(limit: Double)(f: Double => Double) = {
  val values = exps(5).sorted.map(_ + limit)
  values.map(v => (v, f(v)))
}

def pprint(values: Seq[(Double, Double)]): Unit = {
  val maxLength = values.map(_._1.toString.length).max
  val formatString = s"%${maxLength}s"
  values
    .map { case (diff, value) => s"${formatString.format(diff)} | $value" }
    .foreach(println)
}