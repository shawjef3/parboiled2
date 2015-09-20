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

import scala.annotation.tailrec
import java.nio.ByteBuffer
import scodec.bits.ByteVector

trait ParserInput {
  /**
   * Returns the byte at the given (zero-based) index.
   * Note: this method is hot and should be small and efficient.
   * A range-check is not required for the parser to work correctly.
   */
  def byteAt(ix: Int): Byte

  /**
   * The number of bytes in this input.
   * Note: this method is hot and should be small and efficient.
   */
  def length: Int

  /**
   * Returns the characters between index `start` (inclusively) and `end` (exclusively) as a `ByteVector`.
   */
  def sliceVector(start: Int, end: Int): ByteVector

  /**
   * Returns the bytes between index `start` (inclusively) and `end` (exclusively) as an `Array[Byte]`.
   */
  def sliceByteArray(start: Int, end: Int): Array[Byte]

}

object ParserInput {
  val Empty = apply(ByteVector.empty)

  implicit def apply(bytes: ByteVector): ByteVectorBasedParserInput = new ByteVectorBasedParserInput(bytes)
  implicit def apply(bytes: ByteVector, endIndex: Int): ByteVectorBasedParserInput = new ByteVectorBasedParserInput(bytes, endIndex)

  abstract class DefaultParserInput extends ParserInput {

  }

  /**
   * ParserInput reading directly off a byte array.
   * This avoids a separate decoding step but assumes that each byte represents exactly one character,
   * which is encoded by ISO-8859-1!
   * You can therefore use this ParserInput type only if you know that all input will be `ISO-8859-1`-encoded,
   * or only contains 7-bit ASCII characters (which is a subset of ISO-8859-1)!
   *
   * Note that this ParserInput type will NOT work with general `UTF-8`-encoded input as this can contain
   * character representations spanning multiple bytes. However, if you know that your input will only ever contain
   * 7-bit ASCII characters (0x00-0x7F) then UTF-8 is fine, since the first 127 UTF-8 characters are
   * encoded with only one byte that is identical to 7-bit ASCII and ISO-8859-1.
   */
  class ByteVectorBasedParserInput(bytes: ByteVector, endIndex: Int = 0) extends DefaultParserInput {
    val length = if (endIndex <= 0 || endIndex > bytes.length) bytes.length else endIndex
    def byteAt(ix: Int) = bytes(ix)
    def sliceVector(start: Int, end: Int) = bytes.slice(start, end)
    def sliceByteArray(start: Int, end: Int) =
      sliceVector(start, end).toArray
  }

}
