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

sealed abstract class BytePredicate extends (Byte ⇒ Boolean) {
  import BytePredicate._

  def ++(that: BytePredicate): BytePredicate
  def ++(bytes: Seq[Byte]): BytePredicate
  def --(that: BytePredicate): BytePredicate
  def --(bytes: Seq[Byte]): BytePredicate

  def ++(byte: Byte): BytePredicate = this ++ (byte :: Nil)
  def --(byte: Byte): BytePredicate = this -- (byte :: Nil)
  def ++(vector: ByteVector): BytePredicate = this ++ vector.toArray
  def --(vector: ByteVector): BytePredicate = this -- vector.toArray

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
  val LowerAlpha = BytePredicate(('a' to 'z').map(_.toByte))
  val UpperAlpha = BytePredicate(('A' to 'Z').map(_.toByte))
  val Alpha = LowerAlpha ++ UpperAlpha
  val Digit = BytePredicate(('0' to '9').map(_.toByte))
  val Digit19 = BytePredicate(('1' to '9').map(_.toByte))
  val AlphaNum = Alpha ++ Digit
  val LowerHexLetter = BytePredicate(('a' to 'f').map(_.toByte))
  val UpperHexLetter = BytePredicate(('A' to 'F').map(_.toByte))
  val HexLetter = LowerHexLetter ++ UpperHexLetter
  val HexDigit = Digit ++ HexLetter
  val Visible = BytePredicate(('\u0021' to '\u007e').map(_.toByte))
  val Printable = Visible ++ ' '.toByte

  def from(predicate: Byte ⇒ Boolean): BytePredicate =
    predicate match {
      case x: BytePredicate ⇒ x
      case x                ⇒ General(x)
    }

  def apply(magnets: ApplyMagnet*): BytePredicate = (Empty /: magnets) { (a, m) ⇒ a ++ m.predicate }

  class ApplyMagnet(val predicate: BytePredicate)
  object ApplyMagnet {
    implicit def fromPredicate(predicate: Byte ⇒ Boolean): ApplyMagnet = new ApplyMagnet(from(predicate))
    implicit def fromByteVector(vector: ByteVector): ApplyMagnet = fromBytes(vector.toSeq)
    implicit def fromIntegral[I: Integral](b: I*): ApplyMagnet = fromBytes(ByteVector(b: _*).toSeq)
    implicit def fromRange(range: Range): ApplyMagnet = new ApplyMagnet(new RangeBased(range))
    implicit def fromBytes(vector: Seq[Byte]): ApplyMagnet =
      vector match {
        case r: NumericRange[Byte] ⇒ new ApplyMagnet(new RangeBased(new Range(r.start, r.end, r.step)))
        case _                     ⇒ new ApplyMagnet(new ArrayBased(vector.toArray))
      }
  }

  ///////////////////////// PRIVATE ////////////////////////////

  class RangeBased private[BytePredicate] (private val range: Range) extends BytePredicate {
    def apply(c: Byte): Boolean = range contains c.toInt

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

  class ArrayBased private[BytePredicate] (private val bytes: Array[Byte]) extends BytePredicate {
    import java.util.Arrays._
    sort(bytes)

    // TODO: switch to faster binary search algorithm with an adaptive pivot, e.g. http://ochafik.com/blog/?p=106
    def apply(c: Byte): Boolean = binarySearch(bytes, c) >= 0

    def ++(that: BytePredicate): BytePredicate = that match {
      case Empty         ⇒ this
      case x: ArrayBased ⇒ this ++ x.bytes
      case _             ⇒ this or that
    }

    def ++(other: Seq[Byte]): BytePredicate =
      if (other.nonEmpty) new ArrayBased((this -- other).bytes ++ other.toArray[Byte])
      else this

    def --(that: BytePredicate): BytePredicate = that match {
      case Empty         ⇒ this
      case x: ArrayBased ⇒ this -- x.bytes
      case _             ⇒ this andNot that
    }

    def --(other: Seq[Byte]): ArrayBased =
      if (other.nonEmpty) {
        val otherChars = other.toArray
        new ArrayBased(bytes.filter(binarySearch(otherChars, _) < 0))
      } else this

    def intersect(that: BytePredicate): BytePredicate = that match {
      case Empty         ⇒ Empty
      case x: ArrayBased ⇒ new ArrayBased(bytes.intersect(x.bytes))
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
