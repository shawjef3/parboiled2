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
import shapeless.test.illTyped
import org.specs2.specification.Scope

class BasicSpec extends TestParserSpec {

  "The Parser should correctly recognize/reject input for" >> {

    "simple Byte literals" in new TestParser0 {
      val targetRule = rule { 1.toByte }
      ByteVector(1) must beMatched
      ByteVector(0) must beMismatched
      ByteVector.empty must beMismatched
    }

    "a simple Byte `val`" in new TestParser0 {
      val c = 1.toByte
      val targetRule = rule { c }
      ByteVector(1) must beMatched
      ByteVector(0) must beMismatched
      ByteVector.empty must beMismatched
    }

    "a simple Byte `def`" in new TestParser0 {
      def c: Byte = 1
      val targetRule = rule { c }
      ByteVector(1) must beMatched
      ByteVector(0) must beMismatched
      ByteVector.empty must beMismatched
    }

    "simple boxed Byte literals" in new TestParser0 {
      val targetRule = rule { java.lang.Byte.valueOf(1.toByte) }
      ByteVector(1) must beMatched
      ByteVector(0) must beMismatched
      ByteVector.empty must beMismatched
    }

    "a simple boxed Byte `val`" in new TestParser0 {
      val c = java.lang.Byte.valueOf(1.toByte)
      val targetRule = rule { c }
      ByteVector(1) must beMatched
      ByteVector(0) must beMismatched
      ByteVector.empty must beMismatched
    }

    "a simple boxed Byte `def`" in new TestParser0 {
      def c = java.lang.Byte.valueOf(1.toByte)
      val targetRule = rule { c }
      ByteVector(1) must beMatched
      ByteVector(0) must beMismatched
      ByteVector.empty must beMismatched
    }

    "simple Int literals" in new TestParser0 {
      val targetRule = rule { 1 }
      ByteVector(1) must beMatched
      ByteVector(0) must beMismatched
      ByteVector.empty must beMismatched
    }

    "a simple Int `val`" in new TestParser0 {
      val c = 1
      val targetRule = rule { c }
      ByteVector(1) must beMatched
      ByteVector(0) must beMismatched
      ByteVector.empty must beMismatched
    }

    "a simple Int `def`" in new TestParser0 {
      def c = 1
      val targetRule = rule { c }
      ByteVector(1) must beMatched
      ByteVector(0) must beMismatched
      ByteVector.empty must beMismatched
    }

    "a CharPredicate" in new TestParser0 {
      val targetRule = rule { BytePredicate.Digit }
      ByteVector('0'.toByte) must beMatched
      ByteVector('8'.toByte) must beMatched
      ByteVector(0) must beMismatched
      ByteVector.empty must beMismatched
    }

    "anyOf" in new TestParser0 {
      val targetRule = rule { anyOf(0, 1, 2) ~ EOI }
      ByteVector.empty must beMismatched
      ByteVector(0) must beMatched
      ByteVector(1) must beMatched
      ByteVector(2) must beMatched
      ByteVector(3) must beMismatched
      ByteVector(0, 1) must beMismatched
    }

    "noneOf" in new TestParser0 {
      val targetRule = rule { noneOf(0, 1, 2) ~ EOI }
      ByteVector.empty must beMismatched
      ByteVector(0) must beMismatched
      ByteVector(1) must beMismatched
      ByteVector(2) must beMismatched
      ByteVector(3) must beMatched
      ByteVector(0, 1) must beMismatched
    }

    "ANY" in new TestParser0 {
      val targetRule = rule { ANY }
      ByteVector(0) must beMatched
      ByteVector(Byte.MinValue) must beMatched
      ByteVector.empty must beMismatched
    }

    "EOI" in new TestParser0 {
      val targetRule = rule { EOI }
      ByteVector.empty must beMatched
      ByteVector(0) must beMismatched
    }

    "byte ranges" in new TestParser0 {
      val targetRule = rule { (1 - 5) ~ EOI }
      ByteVector(1) must beMatched
      ByteVector(3) must beMatched
      ByteVector(5) must beMatched
      ByteVector.empty must beMismatched
      ByteVector(0) must beMismatched
      ByteVector(-1) must beMismatched
      ByteVector(6) must beMismatched
    }

    "MATCH" in new TestParser0 {
      val targetRule = rule { MATCH ~ EOI }
      ByteVector.empty must beMatched
      ByteVector(0) must beMismatched
    }

    "rule calls to `val`s" in new TestParser0 {
      val targetRule = rule { (val0 | val1(1) | val2(2, 2) | val3(3, ByteVector(3), 4)) ~ EOI }
      val val0 = rule { 0 }
      val val1 = rule[Int]() { _ times 1 }
      val val2 = rule[Int, Byte]() { _ times _ }
      val val3 = rule[Int, ByteVector, Byte]() { (i, s, b) ⇒ i times (s :+ b) }
      ByteVector(0) must beMatched
      ByteVector(1) must beMatched
      ByteVector(2, 2) must beMatched
      ByteVector(3, 4, 3, 4, 3, 4) must beMatched
    }

    "rule calls to `def`s with inner parameters" in new TestParser0 {
      val targetRule = rule { (def0 | def1(1) | def2(2, 2) | def3(3, ByteVector(3), 4)) ~ EOI }
      def def0 = rule { 0 }
      def def1 = rule[Int]() { _ times 1 }
      def def2 = rule[Int, Byte]() { _ times _ }
      def def3 = rule[Int, ByteVector, Byte]() { (i, s, b) ⇒ i times (s :+ b) }
      ByteVector(0) must beMatched
      ByteVector(1) must beMatched
      ByteVector(2, 2) must beMatched
      ByteVector(3, 4, 3, 4, 3, 4) must beMatched
    }

    "rule calls to `def`s with outer parameters" in new TestParser0 {
      val targetRule = rule { (def0 | def1(1) | def2(2, 2) | def3(3, ByteVector(3), 4)) ~ EOI }
      def def0 = rule { '0' }
      def def1(i: Int) = rule { i times 1 }
      def def2(i: Int, c: Byte) = rule { i times c }
      def def3(i: Int, s: ByteVector, c: Byte) = rule { i times (s :+ c) }
      ByteVector(0) must beMatched
      ByteVector(1) must beMatched
      ByteVector(2, 2) must beMatched
      ByteVector(3, 4, 3, 4, 3, 4) must beMatched
    }

    "Map[ByteVector, T]" in new TestParser1[Int] {
      val colors = Map(ByteVector(0) -> 1, ByteVector(-1, 3, 7) -> 2, ByteVector.empty -> 3)
      val targetRule = rule { colors ~ EOI }
      ByteVector(0) must beMatchedWith(1)
      ByteVector(-1, 3, 7) must beMatchedWith(2)
      ByteVector.empty must beMatchedWith(3)
      ByteVector(-1) must beMismatched
    }
  }

  "The Parser" should {
    "disallow compilation of an illegal byte range" in new SimpleParser with Scope {
      illTyped(s"""rule { ${Byte.MinValue.toInt - 1} - 0""", "lower bound must be a byte")
      illTyped(s"""rule { 0 - ${Byte.MaxValue.toInt + 1}""", "upper bound must be a byte")
      illTyped("""rule { 5 - 1 }""", "lower bound must not be > upper bound")
      success
    }
  }
}
