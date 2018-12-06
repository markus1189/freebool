package de.codecentric.freebool

import cats.instances.int._
import cats.instances.option._
import java.time.ZonedDateTime
import spire.std.boolean._
import spire.syntax.heyting._
import cats.syntax.functor._
import cats.syntax.traverse._

trait Analysis {
  def needsProductData(c: FreeBool[Campaign]): Boolean = {
    val occurences = FreeBool.analyze(c) {
      case IsAfter(value)     => 0
      case IsBefore(value)    => 0
      case IsPlacement(value) => 0
      case HasKeyword(value)  => 1
      case InCountry(country) => 0
    }

    occurences > 0
  }
}

sealed trait Campaign
case class IsAfter(value: ZonedDateTime) extends Campaign
case class IsBefore(value: ZonedDateTime) extends Campaign
case class HasKeyword(value: String) extends Campaign
case class IsPlacement(value: Placement) extends Campaign
case class InCountry(country: String) extends Campaign

trait CampaignSytax {
  def isAfter(value: ZonedDateTime): FreeBool[Campaign] = Inject(IsAfter(value))
  def isBefore(value: ZonedDateTime): FreeBool[Campaign] =
    Inject(IsBefore(value))
  def hasKeyword(value: String): FreeBool[Campaign] = Inject(HasKeyword(value))
  def isPlacement(value: Placement): FreeBool[Campaign] =
    Inject(IsPlacement(value))

  def allKeywords(ks: Seq[String]): FreeBool[Campaign] =
    ks.foldLeft(Zero: FreeBool[Campaign])((acc, k) => And(acc, hasKeyword(k)))
}

object Campaign extends CampaignSytax

sealed trait Placement
case object Top extends Placement
case object Bottom extends Placement

object Program extends App with CampaignSytax {
  private val timeCondition = (isBefore(ZonedDateTime.now().plusMinutes(10)) & isAfter(
    ZonedDateTime.now().minusMinutes(10))) | isBefore(
    ZonedDateTime.now().minusDays(365))

  private val keywords = hasKeyword("blackweek") | allKeywords(
    Seq("foo", "bar", "baz"))

  val campaign = timeCondition & keywords & isPlacement(Top)

  val pure = campaign.map {
    case IsAfter(value)     => Zero
    case IsBefore(value)    => One
    case HasKeyword(value)  => Zero
    case IsPlacement(value) => One
    case InCountry(country) => Zero
  }

  val r = campaign.traverse {
    case IsAfter(value)     => Some(One)
    case IsBefore(value)    => Some(One)
    case HasKeyword(value)  => Some(One)
    case IsPlacement(value) => None
    case InCountry(country) => None
  }
}
