package tasks

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayStack => ScalaStack}

// statements #1
object Stack {
  def isBalanced(str: String): String = {
    val open  = Seq('(', '[', '{')
    val close = Seq(')', ']', '}')
    val all   = open ++ close
    type E = (Char, Int)
    @tailrec
    def recur(ch: E, acc: Seq[E], st: ScalaStack[E]): String = ch._1 match {
      case '(' | '[' | '{' =>
        if (acc.nonEmpty) {
          st.push(ch)
          recur(acc.head, acc.tail, st)
        } else {
          (ch._2 + 1).toString
        }
      case _ if st.isEmpty && acc.isEmpty && all.contains(ch._1) =>
        (ch._2 + 1).toString
      case _
          if st.nonEmpty && (
            (ch._1 == ']' && st.top._1 == '[')
            || (ch._1 == ')' && st.top._1 == '(')
            || (ch._1 == '}' && st.top._1 == '{')
          ) =>
        if (acc.isEmpty) {
          st.pop()
          if (st.isEmpty) {
            "Success"
          } else {
            (st.pop()._2 + 1).toString
          }

        } else {
          st.pop()
          recur(acc.head, acc.tail, st)
        }
      case _ =>
        if (all.contains(ch._1)) {
          (ch._2 + 1).toString
        } else {
          acc.headOption match {
            case Some(value) => recur(value, acc.tail, st)
            case None        => "Success"
          }
        }
    }

    if (str.nonEmpty) {
      val seq = str.zipWithIndex
      recur(seq.head, seq.tail, new ScalaStack[E]())
    } else {
      "Success"
    }
  }
}

object Test extends App {
  def test(str: String, expected: String = "Success") = {
    val res = Stack.isBalanced(str)
    println(s"$str -> $expected")
    assert(res == expected, s"$res != $str -> $expected")
  }

  test("[]")
  test("{}[]")
  test("[()]")
  test("(())")
  test("{[]}()")
  test("{", "1")
  test("{[}", "3")
  test("foo(bar);")
  test("foo(bar[i);", "10")
  test("([](){([])})")
  test("()[]}", "5")
  test("{{[()]]", "7")
  test("({[", "3")
  test("({{}", "2")
  Map(
    "((({[]})"             -> "2",
    "{}([]"                -> "3",
    "(slkj, {lk[lve]} ,l)" -> "Success",
    "(slkj{lk[lsj]}"       -> "1",
    "dasdsadsadas]]]"      -> "13"
  ).map {
    case (k, v) => test(k, v)
  }
}

//assert check("([](){([])})") == 0
//assert check("()[]}") == 5
//assert check("{{[()]]") == 7
//assert check("{{{[][][]") == 3
//assert check("{*{{}") == 3
//assert check("[[*") == 2
//assert check("{*}") == 0
//assert check("{{") == 2
//assert check("{}") == 0
//assert check("") == 0
//assert check("}") == 1
//assert check("*{}") == 0
//assert check("{{{**[][][]") == 3

//void TestOpening() {
//ASSERT_EQUAL(IsBalanced({ "{" }).second, "1");
//ASSERT_EQUAL(IsBalanced({ "{[]" }).second, "1");
//ASSERT_EQUAL(IsBalanced({ "{{{" }).second, "3");
//ASSERT_EQUAL(IsBalanced({ "[]([]" }).second, "3");
//ASSERT_EQUAL(IsBalanced({ "{{{[][][]" }).second, "3");
//ASSERT_EQUAL(IsBalanced({ "{{{{{{{((()))}" }).second, "6");
//ASSERT_EQUAL(IsBalanced({ "{()}{" }).second, "5");
//}
//
//void TestClosing() {
//ASSERT_EQUAL(IsBalanced({ "}()" }).second, "1");
//ASSERT_EQUAL(IsBalanced({ "()}()" }).second, "3");
//ASSERT_EQUAL(IsBalanced({ "}()" }).second, "1");
//ASSERT_EQUAL(IsBalanced({ "{[()]}}()" }).second, "7");
//ASSERT_EQUAL(IsBalanced({ "dasdsadsadas]]]" }).second, "13");
//}
//
//void TestSuccess() {
//ASSERT_EQUAL(IsBalanced({ "{}()" }).second, "Success");
//ASSERT_EQUAL(IsBalanced({ "({}[(((())))])" }).second, "Success");
//ASSERT_EQUAL(IsBalanced({ "()" }).second, "Success");
//ASSERT_EQUAL(IsBalanced({ "({})" }).second, "Success");
//ASSERT_EQUAL(IsBalanced({ "foo(bar({ <some initialization> })[i]);" }).second, "Success");
//}
//
//void TestFromStepik() { // https://stepik.org/lesson/41234/step/1?discussion=424085&unit=
//ASSERT_EQUAL(IsBalanced({ "([](){([])})" }).second, "Success");
//ASSERT_EQUAL(IsBalanced({ "()[]}" }).second, "5");
//ASSERT_EQUAL(IsBalanced({ "{{[()]]" }).second, "7");
//ASSERT_EQUAL(IsBalanced({ "{{{[][][]" }).second, "3");
//ASSERT_EQUAL(IsBalanced({ "{*{{}" }).second, "3");
//ASSERT_EQUAL(IsBalanced({ "[[*" }).second, "2");
//ASSERT_EQUAL(IsBalanced({ "{{" }).second, "2");
//ASSERT_EQUAL(IsBalanced({ "{{{**[][][]" }).second, "3");
//}﻿
