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

import scala.annotation.compileTimeOnly
import org.parboiled2.support._
import shapeless.{ HNil, HList }

trait RuleDSLBasics { this: RuleTypes ⇒

  private type Rule0 = Rule[Any, HNil, HNil] // local brevity alias

  /**
   * Returns the [[ParserState]] for the current run.
   */
  @compileTimeOnly("Calls to `state` must be inside `rule` macro")
  implicit def state: ParserState[Context] = `n/a`

  /**
   * Returns the user [[Context]] for the current run. (Shortcut for `state.ctx`)
   */
  @compileTimeOnly("Calls to `ctx` must be inside `rule` macro")
  implicit def ctx: Context = `n/a`

  /**
   * Matches the given single character.
   */
  @compileTimeOnly("Calls to `ch` must be inside `rule` macro")
  implicit def ch(c: Char): Rule0 = `n/a`

  /**
   * Matches the given string of characters.
   */
  @compileTimeOnly("Calls to `str` must be inside `rule` macro")
  implicit def str(s: String): Rule0 = `n/a`

  /**
   * Matches any (single) character matched by the given `CharPredicate`.
   */
  @compileTimeOnly("Calls to `predicate` must be inside `rule` macro")
  implicit def predicate(p: BytePredicate): Rule0 = `n/a`

  /**
   * Matches any of the given maps keys and pushes the respective value upon
   * a successful match.
   */
  @compileTimeOnly("Calls to `valueMap` must be inside `rule` macro")
  implicit def valueMap[T](m: Map[String, T])(implicit h: HListable[T]): Rule[Any, HNil, h.Out] = `n/a`

  /**
   * Matches any single one of the given characters.
   *
   * Note: This helper has O(n) runtime with n being the length of the given string.
   * If your string consists only of 7-bit ASCII chars using a pre-allocated
   * [[BytePredicate]] will be more efficient.
   */
  @compileTimeOnly("Calls to `anyOf` must be inside `rule` macro")
  def anyOf(chars: String): Rule0 = `n/a`

  /**
   * Matches any single character except the ones in the given string and except EOI.
   *
   * Note: This helper has O(n) runtime with n being the length of the given string.
   * If your string consists only of 7-bit ASCII chars using a pre-allocated
   * [[BytePredicate]] will be more efficient.
   */
  @compileTimeOnly("Calls to `noneOf` must be inside `rule` macro")
  def noneOf(chars: String): Rule0 = `n/a`

  /**
   * Matches the given single character case insensitively.
   * Note: the given character must be specified in lower-case!
   * This requirement is currently NOT enforced!
   */
  @compileTimeOnly("Calls to `ignoreCase` must be inside `rule` macro")
  def ignoreCase(c: Char): Rule0 = `n/a`

  /**
   * Matches the given string of characters case insensitively.
   * Note: the given string must be specified in all lower-case!
   * This requirement is currently NOT enforced!
   */
  @compileTimeOnly("Calls to `ignoreCase` must be inside `rule` macro")
  def ignoreCase(s: String): Rule0 = `n/a`

  /**
   * Matches any character except EOI.
   */
  @compileTimeOnly("Calls to `ANY` must be inside `rule` macro")
  def ANY: Rule0 = `n/a`

  /**
   * Matches the EOI (end-of-input) byte.
   */
  def EOI: java.lang.Byte = org.parboiled2.EOI

  /**
   * Matches no character (i.e. doesn't cause the parser to make any progress) but succeeds always (as a rule).
   */
  @compileTimeOnly("Calls to `MATCH` must be inside `rule` macro")
  def MATCH: Rule0 = `n/a`

  /**
   * A Rule0 that always fails.
   */
  @compileTimeOnly("Calls to `MISMATCH0` must be inside `rule` macro")
  def MISMATCH0: Rule0 = `n/a`

  /**
   * A generic Rule that always fails.
   */
  @compileTimeOnly("Calls to `MISMATCH` must be inside `rule` macro")
  def MISMATCH[I <: HList, O <: HList]: Rule[Any, I, O] = `n/a`

  /**
   * A rule that always fails and causes the parser to immediately terminate the parsing run.
   * The resulting parse error only has a single trace with a single frame which holds the given error message.
   */
  def fail(expected: String): Rule0 = `n/a`

  /**
   * Fully generic variant of [[fail]].
   */
  def failX[I <: HList, O <: HList](expected: String): Rule[Any, I, O] = `n/a`

  @compileTimeOnly("Calls to `str2CharRangeSupport` must be inside `rule` macro")
  implicit def str2CharRangeSupport(s: String): CharRangeSupport = `n/a`
  sealed trait CharRangeSupport {
    def -(other: String): Rule0
  }
}
