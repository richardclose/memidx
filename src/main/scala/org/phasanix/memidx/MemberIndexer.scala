package org.phasanix.memidx

/**
  * Macro-generated implementations of `MemberIndexer` will
  * address no-param methods of instances of `A`, giving
  * values of type `R`.
  *
  * @param names names of mapped methods/properties
  * @param conversionsTo converter to return type
  * @tparam A type of target values
  * @tparam R return type
  * @tparam C type of converter to return type
  */
abstract class MemberIndexer[A, R, C <: ConversionsTo[R]](names: Seq[String], val conversionsTo: C) {

  private val nameToIndex = names.zipWithIndex.toMap

  /***
    * Convert a member by index
    * @param value instance of A of which a member is converted
    * @param index index of member to convert
    * @return result of conversion.
    */
  def read(value: A, index: Int): R

  /**
    * Number of mapped methods/properties
    */
  def arity: Int = names.length

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


object MemberIndexer {
  import reflect.macros.blackbox
  import language.experimental.macros

  def create[A, R]: Ctor[A, R] = new Ctor[A, R]

  def createTuple[A <: Product, R]: CtorTuple[A, R] = new CtorTuple[A, R]

  class Ctor[A, R] {
    def apply[C <: ConversionsTo[R]](conversionsTo: C): MemberIndexer[A, R, C] = macro apply_impl[A, R, C]
  }

  class CtorTuple[A, R] {
    def apply[C <: ConversionsTo[R]](conversionsTo: C): MemberIndexer[A, R, C] = macro applyTuple_impl[A, R, C]
  }

  def applyTuple_impl[A: c.WeakTypeTag, R: c.WeakTypeTag, C: c.WeakTypeTag](c: blackbox.Context)(conversionsTo: c.Expr[C]) = {

    import c.universe._

    val typeA = weakTypeOf[A]
    val typeR = weakTypeOf[R]
    val typeC = weakTypeOf[C]

    val types = typeA.typeArgs
    val memberLists = types.map(tpe => mappedMembers(c)(tpe))
    val arities = memberLists.map(_.length)
    val names = for {
      (ms, n) <- memberLists.zipWithIndex
      name <- ms.map(_.name.toString)
    } yield name + "_" + n

    val offsets: Seq[(Int, Int, Int)] = {
      val arr = arities.foldLeft(0 -> Array.empty[Int]) { (acc, e) =>  (e + acc._1, acc._2 :+ (e + acc._1)) }._2
      (0 +: arr).sliding(2).toArray.zipWithIndex.map(x => (x._2, x._1(0), x._1(1)))
    }

    val mis = types.map(t => create(c)(t, typeR, typeC, conversionsTo))

    val cases = offsets.map { case(index, from, to) =>
      val mbrName = TermName("_" + (index + 1))
      cq""" n if n >= $from && n < $to => mis.$mbrName.read(value.$mbrName, n - $from) """
    } :+ cq""" _ => throw new Exception ("index " + index + " out of range") """

    q"""
       new org.phasanix.memidx.MemberIndexer[$typeA, $typeR, $typeC]($names, $conversionsTo)  {
          val mis = (..$mis)

         def read(value: $typeA, index: Int): $typeR = {
           index match {
             case ..$cases
           }
         }
       }
     """
  }

  def apply_impl[A: c.WeakTypeTag, R: c.WeakTypeTag, C: c.WeakTypeTag](c: blackbox.Context)(conversionsTo: c.Expr[C]) =
    create[C](c)(c.weakTypeOf[A], c.weakTypeOf[R], c.weakTypeOf[C], conversionsTo)

  // Select members to map
  private def mappedMembers(c: blackbox.Context)(tpe: c.Type): List[c.universe.MethodSymbol] = {
    val methodsToSkip = Seq("productPrefix", "productArity", "productIterator", "hashCode", "toString").toSet

    val members = tpe.decls
      .collect { case m if m.isMethod && m.asMethod.paramLists.flatten.isEmpty => m.asMethod }

    val isTuple = Seq("_1", "productIterator", "productPrefix")
      .forall(s => members.exists(_.name.toString == s))


    members.filter { m =>
      val name = m.name.toString
      val returnType = m.typeSignatureIn(tpe).finalResultType

      if (methodsToSkip.contains(name) || name.startsWith("copy$default$")) {
        false
      } else if (isTuple) {
        name.startsWith("_")
      } else {
        true
      }
    }.toList
  }

  private def create[C](c: blackbox.Context)(ta: c.Type, tr: c.Type, tc: c.Type, conversionsTo: c.Expr[C]) = {

    import c.universe._

    val methodsToSkip = Seq("productPrefix", "productArity", "productIterator", "hashCode", "toString").toSet

    val props = mappedMembers(c)(ta)

    def accessor(tree: Tree, returnType: Type) = {

      // Specific types first
      if (returnType =:= typeOf[Int]) {
        q"conversionsTo.fromInt($tree)"
      } else if (returnType =:= typeOf[Long]) {
        q"conversionsTo.fromLong($tree)"
      } else if (returnType =:= typeOf[Double]) {
        q"conversionsTo.fromDouble($tree)"
      } else if (returnType =:= typeOf[Float]) {
        q"conversionsTo.fromFloat($tree)"
      } else if (returnType =:= typeOf[Boolean]) {
        q"conversionsTo.fromBoolean($tree)"
      } else if (returnType =:= typeOf[java.time.LocalDate]) {
        q"conversionsTo.fromLocalDate($tree)"
      } else if (returnType =:= typeOf[java.time.LocalDateTime]) {
        q"conversionsTo.fromLocalDateTime($tree)"
      } else if (returnType =:= typeOf[java.util.Date]) {
        q"conversionsTo.fromJUDate($tree)"
      } else if (returnType =:= typeOf[String]) {
        q"conversionsTo.fromString($tree)"
      } else {
        // User-specified type: vs must have an overload for it named "from"
        c.info(NoPosition, s"mapping to conversionsTo.from(value: $returnType) (which must exist)\n" + "" +
          "If compiler warns about structural types, consider named instead of anonymous class", force = true)
        q"conversionsTo.from($tree)"
      }
    }

    val snippets = props.map { m =>
      val returnType = m.typeSignatureIn(ta).finalResultType
      val nme = m.name
      if (returnType <:< typeOf[Option[_]]) {
        val optType = returnType.typeArgs.head
        val acc = accessor(q"e", optType)
        q"{ x: $ta => x.$nme.map(e => $acc).getOrElse(conversionsTo.nilValue) }"
      } else {
        val acc = accessor(q"x.$nme", returnType)
        q"{ x: $ta => $acc }"
      }
    }

    val names = props.map(_.name.toString)

    val arity = names.length

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