package com.gu.zuora.retention

import com.gu.zuora.retention.CaseInsensitiveId.{Comparison, Equal, OtherThenThis, ThisThenOther}
import com.gu.zuora.retention.SortedIterator.IsContained

object Diff {

  /**
   * Returns an iterator for the lines in candidateLines with crmIds that are not in the exclusionLines
   * The candidates and exclusion iterators are expected to iterate ascending CrmId order.
   * The point of this is to avoid loading the whole exclusionLines in memory.
   */
  def apply(candidateLines: Iterator[String], exclusionLines: Iterator[String]): Iterator[String] = {

    val candidateCrmLines = CrmIdCSVIterator(candidateLines)

    // this var requires the filter to happen in order (ok because it's an iterator)
    // also, the sorted iterator should be overwritten so it can be GCed.
    var mutableExclusionCrmIdsIter = SortedIterator.fromIterWithHeader(exclusionLines)
    candidateCrmLines.filterNotIds { crmId =>

      val IsContained(exclusionsWithEarlierDropped, shouldDropCandidateLine) =
        mutableExclusionCrmIdsIter.contains(_.compare(crmId))

      mutableExclusionCrmIdsIter = exclusionsWithEarlierDropped // !!

      shouldDropCandidateLine
    }
  }

}

object CrmIdCSVIterator {

  val crmIdColName = "Account.CrmId"

  case class CrmLine(crmId: CaseInsensitiveId, rawLine: String)
  case class HeaderIter(csvHeader: String, csvIter: Iterator[CrmLine]) {

    // MUST only call once
    def filterNotIds(f: CaseInsensitiveId => Boolean): Iterator[String] = {
      List(csvHeader).iterator ++
        csvIter.filterNot(line => f(line.crmId)).map(_.rawLine)
    }

  }

  def apply(rest: Iterator[String]): HeaderIter = {
    val header = rest.next
    val crmidLocation = header.split(",").indexOf(crmIdColName)
    val iter = rest.collect {
      case line if !line.trim.isEmpty =>
        val crmId = line.split(",")(crmidLocation).trim
        CrmLine(CaseInsensitiveId(crmId), line)
    }
    HeaderIter(header, iter)
  }

}

case class CaseInsensitiveId(value: String) {

  def compare(other: CaseInsensitiveId): Comparison =
    if (value == other.value)
      Equal
    else if (value < other.value)
      ThisThenOther
    else
      OtherThenThis

}
object CaseInsensitiveId {
  def apply(raw: String): CaseInsensitiveId = new CaseInsensitiveId(value = raw.toLowerCase)

  sealed trait Comparison
  case object ThisThenOther extends Comparison
  case object Equal extends Comparison
  case object OtherThenThis extends Comparison
}

class SortedIterator(private val crmIdIterator: Iterator[CaseInsensitiveId]) {

  // due to this line being lazy, it will only "next" the iterator once
  // we can go back up the stack and the value will still be correct, however that would
  // be silly as the whole iterable would be in memory on the stack.
  // this means it should always be used tail recursively to allow the
  // garbage collector to collect the earlier parts.
  private lazy val peekNext: Option[CaseInsensitiveId] = if (crmIdIterator.hasNext) Some(crmIdIterator.next) else None

  // warning - you are only allowed to call contains once, then you MUST use the returned "rest" iterator
  def contains(f: CaseInsensitiveId => Comparison): IsContained =
    peekNext.map(f) match {
      case Some(ThisThenOther) => new SortedIterator(crmIdIterator).contains(f)
      case Some(OtherThenThis) => IsContained(this, isContained = false)
      case Some(Equal) => IsContained(this, isContained = true)
      case None => IsContained(this, isContained = false)
    }

}
object SortedIterator {

  case class IsContained(rest: SortedIterator, isContained: Boolean)

  def fromIterWithHeader(crmIdIterator: Iterator[String]): SortedIterator = {
    crmIdIterator.next //skip header
    new SortedIterator(crmIdIterator.map(CaseInsensitiveId.apply))
  }

}
