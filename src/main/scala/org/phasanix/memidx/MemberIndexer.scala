package org.phasanix.memidx

trait MemIdx[A, R] {
  /**
    * Read a value by index
    * @param value
    * @param index
    * @return
    */
  def read(value: A, index: Int): R

  /**
    * Names of mapped properties, in declaration order
    */
  val names: Seq[String]

  /**
    * Number of mapped properties
    * @return
    */
  def arity: Int

  private lazy val nameToIndex = names.zipWithIndex.toMap

  /**
    * Convert a member by name
    * @param value instance of A of which a member is converted
    * @param name name of member to convert
    * @return result of conversion.
    */
  def read(value: A, name: String): Option[R] = nameToIndex.get(name).map(read(value, _))

  /** member iterator */
  def members(value: A): Iterator[R] = new Iterator[R] {
    var pos: Int = 0
    def hasNext: Boolean = pos < arity

    def next(): R = {
      val ret = read(value, pos)
      pos += 1
      ret
    }
  }

}

/**
  *
  * @param names list of names of methods of type A
  * @param conv instance of ConversionsTo[R]
  * @tparam A type whose properties are read
  * @tparam R type to which all values are converted
  * @tparam C type of implementation of ConversionsTo[R]
  */
abstract class MemberIndexer[A, R, C <: ConversionsTo[R]](val names: Seq[String], val conv: C) extends MemIdx[A, R] {

  /***
    * Convert a member by index
    * @param value instance of A of which a member is converted
    * @param index index of member to convert
    * @return result of conversion.
    */
  def read(value: A, index: Int): R

  def readAny(value: Any, index: Int): R = read(value.asInstanceOf[A], index)

  /**
    * Number of methods mapped
    */
  def arity: Int = names.length
}

class MemberIndexerSet[A <: Product, R, C <: ConversionsTo[R]](mis: Seq[MemberIndexer[_, R, C]])
  extends MemIdx[A, R] {

  private val offsets: Array[(Int, Int, Int)] = {
    val arr = mis.map(_.arity).foldLeft(0 -> Array.empty[Int]) { (acc, e) =>  (e + acc._1, acc._2 :+ (e + acc._1)) }._2
    (0 +: arr).sliding(2).toArray.zipWithIndex.map(x => (x._2, x._1(0), x._1(1)))
  }

  def read(value: A, index: Int): R = {
    offsets
      .collect { case (i, from, to) if index >= from && index < to =>
        mis(i).readAny(value.productElement(i), index - from)
      }
      .headOption
      .getOrElse(throw new Exception(s"index $index out of range"))
  }

  val names: Seq[String] =
    for {
      (mi, i) <- mis.zipWithIndex
      name <- mi.names
    } yield {
      name + "_" + i
    }

  def arity: Int = mis.map(_.arity).sum
}

object MemberIndexer {
  import reflect.macros.blackbox
  import language.experimental.macros

  def create[A, R]: Ctor[A, R] = new Ctor[A, R]

  def createTuple[A <: Product, R]: CtorTuple[A, R] = new CtorTuple[A, R]

  class Ctor[A, R] {
    def apply[C <: ConversionsTo[R]](conversionsTo: C): MemberIndexer[A, R, C] = macro apply_impl[A, R, C]
  }

  class CtorTuple[A, R] {
    def apply[C <: ConversionsTo[R]](conversionsTo: C): MemberIndexerSet[A, R, C] = macro applyTuple_impl[A, R, C]
  }

  def applyTuple_impl[A: c.WeakTypeTag, R: c.WeakTypeTag, C: c.WeakTypeTag](c: blackbox.Context)(conversionsTo: c.Expr[C]) = {

    import c.universe._

    val mis = weakTypeOf[A].typeArgs.map { t =>
      create(c)(t, weakTypeOf[R], weakTypeOf[C], conversionsTo)
    }

    q"new org.phasanix.memidx.MemberIndexerSet(Seq(..$mis))"
  }

  def apply_impl[A: c.WeakTypeTag, R: c.WeakTypeTag, C: c.WeakTypeTag](c: blackbox.Context)(conversionsTo: c.Expr[C]) =
    create[C](c)(c.weakTypeOf[A], c.weakTypeOf[R], c.weakTypeOf[C], conversionsTo)

  private def create[C](c: blackbox.Context)(ta: c.Type, tr: c.Type, tc: c.Type, conversionsTo: c.Expr[C]) = {

    import c.universe._

    val methodsToSkip = Seq("productPrefix", "productArity", "productIterator", "hashCode", "toString").toSet

    val members = ta.decls
      .collect { case m: MethodSymbol if m.paramLists.flatten.isEmpty => m }

    val isTuple = Seq("_1", "productIterator", "productPrefix")
      .forall(s => members.exists(_.name.toString == s))

    val props =  members.filter { m =>
      val name = m.name.toString
      val returnType = m.typeSignatureIn(ta).finalResultType

      if (methodsToSkip.contains(name) || name.startsWith("copy$default$")) {
        false
      } else if (isTuple) {
        name.startsWith("_")
      } else {
        true
      }
    }.toSeq

    def accessor(tree: Tree, returnType: Type) = {

      // Specific types first
      if (returnType =:= typeOf[Int]) {
        q"conv.fromInt($tree)"
      } else if (returnType =:= typeOf[Long]) {
        q"conv.fromLong($tree)"
      } else if (returnType =:= typeOf[Double]) {
        q"conv.fromDouble($tree)"
      } else if (returnType =:= typeOf[Float]) {
        q"conv.fromFloat($tree)"
      } else if (returnType =:= typeOf[Boolean]) {
        q"conv.fromBoolean($tree)"
      } else if (returnType =:= typeOf[java.time.LocalDate]) {
        q"conv.fromLocalDate($tree)"
      } else if (returnType =:= typeOf[java.time.LocalDateTime]) {
        q"conv.fromLocalDateTime($tree)"
      } else if (returnType =:= typeOf[java.util.Date]) {
        q"conv.fromJUDate($tree)"
      } else if (returnType =:= typeOf[String]) {
        q"conv.fromString($tree)"
      } else {
        // User-specified type: vs must have an overload for it named "from"
        c.info(NoPosition, s"mapping to conv.from(value: $returnType) (which must exist)\n" + "" +
          "If compiler warns about structural types, consider named instead of anonymous class", force = true)
        q"conv.from($tree)"
      }
    }

    val snippets = props.map { m =>
      val returnType = m.typeSignatureIn(ta).finalResultType
      val nme = m.name
      if (returnType <:< typeOf[Option[_]]) {
        val optType = returnType.typeArgs.head
        val acc = accessor(q"e", optType)
        q"{ x: $ta => x.$nme.map(e => $acc).getOrElse(conv.nilValue) }"
      } else {
        val acc = accessor(q"x.$nme", returnType)
        q"{ x: $ta => $acc }"
      }
    }

    val names = props.map(_.name.toString)

    val arity = snippets.length

    q"""
       new org.phasanix.memidx.MemberIndexer[$ta,$tr,$tc](Seq(..$names), $conversionsTo) {
          val getters: Seq[($ta => $tr)] = Seq(..$snippets)
          def read(value: $ta, index: Int): $tr = {
            if (index < 0 || index > $arity)
              throw new Exception("index " + index + " out of range (" + arity + ")")
            getters(index)(value)
          }
       }
      """

  }
}