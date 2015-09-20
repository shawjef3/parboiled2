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

import org.specs2.mutable.Specification
import scodec.bits.ByteVector

class BytePredicateSpec extends Specification {

  "BytePredicates" should {

    "support `testAny`" in {
      BytePredicate(1, 2, 3).matchesAny(ByteVector(4, 5, 6)) must beFalse
      BytePredicate(1, 2, 3).matchesAny(ByteVector(4, 5, 2, 6)) must beTrue
    }

    "support `indexOfFirstMatch`" in {
      BytePredicate(1, 2, 3).indexOfFirstMatch(ByteVector(4, 5, 6)) === -1
      BytePredicate(1, 2, 3).indexOfFirstMatch(ByteVector(4, 5, 2, 6)) === 2
    }

    "be backed by an array where possible" in {
      BytePredicate(0, 1, 2, 4, 5, 6).toString === "BytePredicate.ArrayBased(0, 1, 2, 4, 5, 6)"
      (BytePredicate(0, 1, 2, 4, 5, 6) -- BytePredicate(5, 6)).toString === "BytePredicate.ArrayBased(0, 1, 2, 4)"
    }

    "be backed by a range where possible" in {
      BytePredicate(1 to 100).toString === "BytePredicate.RangeBased(1 to 100)"
    }
  }

}
