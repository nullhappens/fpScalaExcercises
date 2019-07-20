package chapter8

import chapter8.Prop.{FailedCase, SuccessCount}

sealed trait Result extends Product with Serializable {
  def isFalsified: Boolean
}
case object Passed extends Result {
  override def isFalsified: Boolean = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount)
    extends Result {
  override def isFalsified: Boolean = true
}

case object Proved extends Result {
  override def isFalsified: Boolean = false
}
