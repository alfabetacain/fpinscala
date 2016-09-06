package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): 
    ParserOps[String] = ParserOps(f(a))
  def orString(s1: String, s2: String): Parser[String]
  def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)] = {
    p.flatMap(a => p2.map(b => (a,b)))
  }

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] =
    a.flatMap(x => succeed(f(x)))

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def slice[A](p: Parser[A]): Parser[String]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    def generator(count: Int): Parser[List[A]] = {
      if (count == 0)
        succeed(List())
      else{
        map2(p, generator(count-1))(_ :: _)
      }
    }
    generator(n)
  }

  implicit def regex(r: Regex): Parser[String]

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def succeed[A](a: A): Parser[A] = 
    map(string(""))(_ => a)

  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] = {
    p.flatMap(a => p2.map(b => f(a,b)))
  }

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = 
    map(string(c.toString))(_.charAt(0))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
  }

  object Laws {
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}
