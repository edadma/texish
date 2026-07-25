package io.github.edadma.texish.parser

import io.github.edadma.texish.TexishException
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Environments: `\newenvironment name {begin-code} {end-code}` defines a LaTeX-style environment, and
  * `\begin{name}` … `\end{name}` runs the begin-code, then the body, then the end-code. The body is a scope: the
  * begin-code opens it and the end-code runs just before it closes, so locals set by the begin-code (or the body)
  * are visible to the end-code and dropped on exit, while `\global` still escapes.
  */
class EnvironmentTests extends AnyFreeSpec with Matchers:

  private def run(src: String): String =
    val h    = new StringHandler
    val proc = new Processor(h)
    proc.process(src)
    h.result

  "\\begin/\\end run the begin-code and end-code around the body" in {
    run("\\newenvironment quote {[}{]}\\begin{quote}x\\end{quote}") shouldBe "[x]"
  }

  "the body is scoped — a local set inside does not leak out" in {
    run("\\set v {out}\\newenvironment e {\\set v {in}}{}\\begin{e}\\v\\end{e}-\\v") shouldBe "in-out"
  }

  "\\global escapes the environment's scope" in {
    run("\\newenvironment e {}{}\\begin{e}\\global\\set w {esc}\\end{e}\\w") shouldBe "esc"
  }

  "the end-code sees a local set by the begin-code" in {
    run("\\newenvironment e {\\set t {T}}{\\t}\\begin{e}\\end{e}") shouldBe "T"
  }

  "environments nest" in {
    run("\\newenvironment a {(}{)}\\newenvironment b {[}{]}\\begin{a}\\begin{b}x\\end{b}\\end{a}") shouldBe "([x])"
  }

  "an unknown environment is an error" in {
    a[TexishException] should be thrownBy run("\\begin{nope}x\\end{nope}")
  }

  "an environment can take arguments, bound in the begin-code" in {
    run("\\newenvironment named title {\\title: }{}\\begin{named}{Lemma}x\\end{named}") shouldBe "Lemma: x"
  }

  "multiple arguments substitute in order" in {
    run("\\newenvironment pair a b {\\a-\\b }{}\\begin{pair}{one}{two}body\\end{pair}") shouldBe "one-two body"
  }

  "\\end must match the innermost \\begin" in {
    a[TexishException] should be thrownBy run(
      "\\newenvironment a {(}{)}\\newenvironment b {[}{]}\\begin{a}\\begin{b}x\\end{a}\\end{b}",
    )
  }

  "an \\end with no open environment is an error" in {
    a[TexishException] should be thrownBy run("\\newenvironment e {}{}\\end{e}")
  }
