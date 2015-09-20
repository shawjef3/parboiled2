/*
 * Copyright (C) 2009-2013 Mathias Doenitz, Alexander Myltsev
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.parboiled2

import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange

import java.lang.Byte

sealed abstract class BytePredicate extends (Byte ⇒ Boolean) {
  import BytePredicate._

  def ++(that: BytePredicate): BytePredicate
  def --(that: BytePredicate): BytePredicate
  def ++(bytes: Seq[Byte]): BytePredicate
  def --(bytes: Seq[Byte]): BytePredicate

  def ++(byte: Byte): BytePredicate = this ++ Seq(byte)
  def --(byte: Byte): BytePredicate = this -- Seq(byte)
  def ++(byte: scala.Byte): BytePredicate = this ++ Byte.valueOf(byte)
  def --(byte: scala.Byte): BytePredicate = this -- Byte.valueOf(byte)
  def ++(vector: ByteVector): BytePredicate = this ++ vector.toSeq.map(Byte.valueOf)
  def --(vector: ByteVector): BytePredicate = this -- vector.toSeq.map(Byte.valueOf)

  def intersect(that: BytePredicate): BytePredicate

  def negated: BytePredicate = this match {
    case Empty ⇒ All
    case All   ⇒ Empty
    case x     ⇒ from(c ⇒ !x(c))
  }

  def matchesAny(vector: ByteVector): Boolean = {
    @tailrec def rec(ix: Int): Boolean =
      if (ix == vector.length) false else if (this(vector(ix))) true else rec(ix + 1)
    rec(0)
  }

  def matchesAll(vector: ByteVector): Boolean = {
    @tailrec def rec(ix: Int): Boolean =
      if (ix == vector.length) true else if (!this(vector(ix))) false else rec(ix + 1)
    rec(0)
  }

  def indexOfFirstMatch(vector: ByteVector): Int = {
    @tailrec def rec(ix: Int): Int =
      if (ix == vector.length) -1 else if (this(vector(ix))) ix else rec(ix + 1)
    rec(0)
  }

  def indexOfFirstMismatch(vector: ByteVector): Int = {
    @tailrec def rec(ix: Int): Int =
      if (ix == vector.length) -1 else if (this(vector(ix))) rec(ix + 1) else ix
    rec(0)
  }

  def firstMatch(vector: ByteVector): Option[Byte] =
    indexOfFirstMatch(vector) match {
      case -1 ⇒ None
      case ix ⇒ Some(vector(ix))
    }

  def firstMismatch(vector: ByteVector): Option[Byte] =
    indexOfFirstMismatch(vector) match {
      case -1 ⇒ None
      case ix ⇒ Some(vector(ix))
    }

  protected def or(that: Byte ⇒ Boolean): BytePredicate =
    from(if (this == Empty) that else c ⇒ this(c) || that(c))
  protected def and(that: Byte ⇒ Boolean): BytePredicate =
    if (this == Empty) Empty else from(c ⇒ this(c) && that(c))
  protected def andNot(that: Byte ⇒ Boolean): BytePredicate =
    from(if (this == Empty) c ⇒ !that(c) else c ⇒ this(c) && !that(c))
}

object BytePredicate {

  val Empty: BytePredicate = from(_ ⇒ false)
  val All: BytePredicate = from(_ ⇒ true)
  val LowerAlpha = BytePredicate(new RangeBased('a'.toInt to 'z'.toInt))
  val UpperAlpha = BytePredicate(new RangeBased('A'.toInt to 'Z'.toInt))
  val Alpha = LowerAlpha ++ UpperAlpha
  val Digit = BytePredicate(new RangeBased('0'.toInt to '9'.toInt))
  val Digit19 = BytePredicate(new RangeBased('1'.toInt to '9'.toInt))
  val AlphaNum = Alpha ++ Digit
  val LowerHexLetter = BytePredicate(new RangeBased('a'.toInt to 'f'.toInt))
  val UpperHexLetter = BytePredicate(new RangeBased('A'.toInt to 'F'.toInt))
  val HexLetter = LowerHexLetter ++ UpperHexLetter
  val HexDigit = Digit ++ HexLetter
  val Visible = BytePredicate(new RangeBased('\u0021'.toInt to '\u007e'.toInt))
  val Printable = Visible ++ ' '.toByte

  def from(predicate: Byte ⇒ Boolean): BytePredicate =
    predicate match {
      case x: BytePredicate ⇒ x
      case x                ⇒ General(x)
    }

  def apply(magnets: ApplyMagnet*): BytePredicate = (Empty /: magnets) { (a, m) ⇒ a ++ m.predicate }

  class ApplyMagnet(val predicate: BytePredicate)
  object ApplyMagnet {
    implicit def fromIntegral[N: Numeric](n: N): ApplyMagnet = fromBytes(Seq(implicitly[Numeric[N]].toInt(n).toByte))
    implicit def fromBoxedByte(byte: Byte): ApplyMagnet = fromBoxedBytes(Seq(byte))
    implicit def fromPredicate(predicate: Byte ⇒ Boolean): ApplyMagnet = new ApplyMagnet(from(predicate))
    implicit def fromByteVector(vector: ByteVector): ApplyMagnet = fromBytes(vector.toSeq)
    implicit def fromRange(range: Range): ApplyMagnet = new ApplyMagnet(new RangeBased(range))
    implicit def fromBytes(vector: Seq[scala.Byte]): ApplyMagnet =
      vector match {
        case r: NumericRange[scala.Byte] ⇒ new ApplyMagnet(new RangeBased(Range(r.start.toInt, r.end.toInt, r.step.toInt)))
        case _                           ⇒ fromBoxedBytes(vector.map(Byte.valueOf))
      }
    implicit def fromBoxedBytes(vector: Seq[Byte]): ApplyMagnet =
      vector match {
        case r: NumericRange[Byte] ⇒ new ApplyMagnet(new RangeBased(Range(r.start.toInt, r.end.toInt, r.step.toInt)))
        case _                     ⇒ new ApplyMagnet(new ArrayBased(vector.toArray))
      }
  }

  ///////////////////////// PRIVATE ////////////////////////////

  class RangeBased private[BytePredicate] (private val range: Range) extends BytePredicate {
    def apply(b: Byte): Boolean = range contains b.toInt

    def ++(that: BytePredicate): BytePredicate = that match {
      case Empty ⇒ this
      case _     ⇒ this or that
    }

    def ++(other: Seq[Byte]): BytePredicate = if (other.nonEmpty) this ++ BytePredicate(other) else this

    def --(that: BytePredicate): BytePredicate = that match {
      case Empty ⇒ this
      case _     ⇒ this andNot that
    }

    def --(other: Seq[Byte]): BytePredicate = if (other.nonEmpty) this -- BytePredicate(other) else this

    def intersect(that: BytePredicate): BytePredicate = that match {
      case Empty ⇒ Empty
      case _     ⇒ this and that
    }

    override def toString(): String = s"CharPredicate.RangeBased(start = ${range.start}, end = ${range.end}, " +
      s"step = ${range.step.toInt}, inclusive = ${range.isInclusive})"
  }

  class ArrayBased private[BytePredicate] (private val boxedBytes: Array[java.lang.Byte]) extends BytePredicate {
    import java.util.Arrays._
    val eoiCount = boxedBytes.filter(_ == null).size // .count(null) throws NullPointerException
    val hasEOI = eoiCount > 0

    private val bytes = Array.ofDim[scala.Byte](boxedBytes.length - eoiCount)

    {
      var i = 0
      var boxedI = 0
      while (boxedI < boxedBytes.length) {
        if (boxedBytes(boxedI) != null) {
          bytes(i) = boxedBytes(boxedI)
          i += 1
        }
        boxedI += 1
      }
    }

    sort(bytes)

    def apply(c: Byte): Boolean = {
      (c == null && hasEOI) ||
        binarySearch(bytes, c.byteValue()) >= 0
    }

    def ++(that: BytePredicate): BytePredicate = that match {
      case Empty         ⇒ this
      case x: ArrayBased ⇒ this ++ boxedBytes
      case _             ⇒ this or that
    }

    def ++(other: Seq[Byte]): BytePredicate =
      if (other.nonEmpty) {
        val union = boxedBytes.toSet ++ other.toSet
        new ArrayBased(union.toArray)
      } else this

    def --(that: BytePredicate): BytePredicate = that match {
      case Empty         ⇒ this
      case x: ArrayBased ⇒ this -- x.boxedBytes
      case _             ⇒ this andNot that
    }

    def --(other: Seq[Byte]): ArrayBased =
      if (other.nonEmpty) {
        val difference = boxedBytes.toSet -- other.toSet
        new ArrayBased(difference.toArray)
      } else this

    def intersect(that: BytePredicate): BytePredicate = that match {
      case Empty         ⇒ Empty
      case x: ArrayBased ⇒
        val intersection = boxedBytes.toSet.intersect(x.boxedBytes.toSet)
        new ArrayBased(intersection.toArray)
      case _             ⇒ this and that
    }

    override def toString(): String = "CharPredicate.ArrayBased(" + new String(bytes) + ')'
  }

  case class General private[BytePredicate] (predicate: Byte ⇒ Boolean) extends BytePredicate {
    def apply(c: Byte) = predicate(c)

    def ++(that: BytePredicate): BytePredicate = that match {
      case Empty                  ⇒ this
      case General(thatPredicate) ⇒ from(c ⇒ predicate(c) || thatPredicate(c))
      case _                      ⇒ from(c ⇒ predicate(c) || that(c))
    }

    def ++(chars: Seq[Byte]): BytePredicate =
      if (chars.nonEmpty) {
        val abp = new ArrayBased(chars.toArray)
        from(c ⇒ predicate(c) || abp(c))
      } else this

    def --(that: BytePredicate): BytePredicate = that match {
      case Empty                  ⇒ this
      case General(thatPredicate) ⇒ from(c ⇒ predicate(c) && !thatPredicate(c))
      case _                      ⇒ from(c ⇒ predicate(c) && !that(c))
    }

    def --(chars: Seq[Byte]): BytePredicate =
      if (chars.nonEmpty) {
        val abp = new ArrayBased(chars.toArray)
        from(c ⇒ predicate(c) && !abp(c))
      } else this

    def intersect(that: BytePredicate) = that match {
      case Empty                  ⇒ Empty
      case General(thatPredicate) ⇒ from(c ⇒ predicate(c) && that(c))
      case _                      ⇒ this and that
    }

    override def toString(): String = "CharPredicate.General@" + System.identityHashCode(this)
  }
}
