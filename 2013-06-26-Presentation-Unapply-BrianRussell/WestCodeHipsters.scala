//
// West Code Choppers - unapply
//

package com._601

object WestCodeHipsters extends App
{
  case class Named( name: String, list: List[Any] )

  // "case" auto-magically creates an object unapply($: Named) = Some($.name, $.list)

  val aList = List(600, 601, 602, "huh?", -601, true)

  // remove type info
  val x: Any = Some( Named( "aList", aList ) )

  //
  // 1. match case

  println("1. val x: Any = " + x)

  // a typical process
  def process(i: Int, s: String) { println(s"process $i of $s") }

  // extract name and second-list-item-Int to process
  // may throw the scala.MatchError RuntimeException
  x match { case Some( Named( nom, List(_, dci: Int, _*) ) ) => process(dci, nom) }

  //
  // 2. variable definition

  // may throw the scala.MatchError RuntimeException
  val Some( Named(_, xList) ) = x

  println("2. val xList = " + xList + " // very equal? " + (xList eq aList))

  //
  // 3. partial function case

  object Exc601 { def unapply(i: Int) = if (i == 601) None else Some(i) }

  // sum of Int except 601
  process( (xList collect { case Exc601(i) => i }).sum, "sum-P" )

  // sans unapply for comparison
  process( (xList collect { case i: Int if i != 601 => i }).sum, "sum-NP" )

  //
  // 4. for expression pattern

  val ra = Array[AnyRef](

    Named("0", Nil),
    Named("1", List("One")),
    Named("t", List("tea", "tee")),
    Named("3", List("West", "Code", "Hipsters")),
    Named("x", List(false, 601.0, true)),
    Named("5", List(true, BigInt(601), true))
  )

  // an extractor (name, isDigital, last?)
  object DigIt { def unapply($: Named) = Some($.name, $.name.forall(_.isDigit), $.list.lastOption) }

  val yields = for ( DigIt(nom, true, Some(fin: String)) <- ra ) yield (nom, fin)

  println("val yields = " + yields.toList)

  // sans unapply for comparison
  val build = for { z <- ra if z.isInstanceOf[Named]

    named = z.asInstanceOf[Named]
    if named.name.forall(_.isDigit)
    if named.list.nonEmpty
    if named.list.last.isInstanceOf[String]
  }
  yield (named.name, named.list.last.asInstanceOf[String])

  println("val builds = " + build.toList)

  println(for (Named(_, List(_, gc, true)) <- ra.toList) yield gc.getClass)
}
/* output
1. val x: Any = Some(Named(aList,List(600, 601, 602, huh?, -601, true)))
process 601 of aList

2. val xList = List(600, 601, 602, huh?, -601, true) // very equal? true

process 601 of sum-P
process 601 of sum-NP

val yields = List((1,One), (3,Hipsters))
val builds = List((1,One), (3,Hipsters))

List(class java.lang.Double, class scala.math.BigInt)
*/
