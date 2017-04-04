package fpinscala.parsing

import language.higherKinds
import scala.util.matching.Regex

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Err, Parser[+_]] (P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._
    val spaces: Parser[String] = operators(char(' ')).many.slice

    val QUOTED: Parser[String] =
      """"([^"]*)"""".r
        .map { _ dropRight 1 substring 1 }

    val DOUBLE: Parser[Double] =
      """\d+(\.\d+)?""".r
        .map { _.toDouble }

    val WS: Parser[Unit] =
      "[\t\n ]+".r map { _ => () }

    val jnull: Parser[JSON] =
      "null" |* succeed (JNull)

    val jbool: Parser[JBool] =
      (WS.? |* "true"  |* succeed(JBool(true))) |
      (WS.? |* "false" |* succeed(JBool(false)))

    val jstring: Parser[JString] =
      QUOTED map { JString(_) }

    val jnumber: Parser[JNumber] =
      DOUBLE map { JNumber(_) }

    lazy val jarray: Parser[JArray] =
      (
        WS.? |* "[" |* (json *| "," ).*
        *| WS.? *| "]" *| WS.?
      ) map { l => JArray(l.toVector) }

    lazy val field: Parser[(String, JSON)] =
      WS.? |* QUOTED *| WS.? *| ":" *| WS.? ** json *| ","

    lazy val jobject: Parser[JObject] =
      (WS.? |* "{" |* field.* *| WS.? *| "}" *| WS.?)
        .map { l => JObject(l.toMap) }

    lazy val json: Parser[JSON] =
      ( jstring | jobject | jarray | jnull | jnumber | jbool) *| WS.?

    return json
  }
}

trait Parsers[ParseError, Parser[+_]] { self => // so inner classes may call methods of trait

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

  def option[A](p: Parser[A]): Parser[Option[A]]

  def orL[A, B](p: Parser[A], p2: Parser[B]): Parser[A]
  def orR[A, B](p: Parser[A], p2: Parser[B]): Parser[B]

  def tuple[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)] =
    map2(p, p2)((a, b) => (a, b))

  case class ParserOps[A](p: Parser[A]) {
    def ?(): Parser[Option[A]] = self.option(p)
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def *|[B](p2: Parser[B]): Parser[A] = self.orL(p,p2)
    def |*[B](p2: Parser[B]): Parser[B] = self.orR(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def *(): Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.tuple(p,p2)
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
