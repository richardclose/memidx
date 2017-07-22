package org.phasanix.memidx

/**
  * Macro-generated implementations of `MemberIndexer` will
  * address no-param methods of instances of `A` by index,
  * giving values of type `R`.
  *
  * @tparam A type of target values
  * @tparam R return type
  */
trait MemberIndexer[A, R] {

  /** names of mapped members */
  val names: Seq[String]

  /** read the indexth member of value, giving an instance of R */
  def read(value: A, index: Int): R

  /** read the member of value which is named 'name', giving an instance of R */
  def read(value: A, name: String): Option[R]

  /** iterator over members, producing instances of R */
  def members(value: A): Iterator[R]

  /** number of mapped members */
  def arity: Int

  /** create builder for a MemberIndexerView */
  def view: MemberIndexerView.Builder[A, R]
}

/**
  * Base class of macro-generated implementation.
  * @param names names of mapped members
  * @param conversionsTo object with conversion methods to type R
  * @tparam A type of target values
  * @tparam R return type
  * @tparam C type of converter to return type
  */
abstract class MemberIndexerImpl[A, R, C <: ConversionsTo[R]]
  (val names: Seq[String], val conversionsTo: C) extends MemberIndexer[A, R] {

  private val namesToIndex = names.zipWithIndex.toMap

  /** Read the indexth member of value */
  def read(value: A, index: Int): R

  /** Read member by name */
  def read(value: A, name: String): Option[R] =
    namesToIndex.get(name).map(read(value, _))

  /** member iterator */
  def members(value: A): Iterator[R] = new Iterator[R] {
    var pos: Int = 0
    def hasNext: Boolean = pos < names.length

    def next(): R = {
      val ret = read(value, pos)
      pos += 1
      ret
    }
  }

  /** Number of mapped members */
  def arity: Int = names.length

  /** Create view builder */
  def view: MemberIndexerView.Builder[A, R] =
    new MemberIndexerView.Builder[A, R](this)
}


object MemberIndexer {
  import reflect.macros.blackbox
  import language.experimental.macros

  /** create MemberIndex instance */
  def create[A, R]: Ctor[A, R] = new Ctor[A, R]

  /** create MemberIndexer instance for tuple of objects */
  def createTuple[A <: Product, R]: CtorTuple[A, R] = new CtorTuple[A, R]

  class Ctor[A, R] {
    def apply[C <: ConversionsTo[R]](conversionsTo: C): MemberIndexerImpl[A, R, C] = macro apply_impl[A, R, C]
  }

  class CtorTuple[A, R] {
    def apply[C <: ConversionsTo[R]](conversionsTo: C): MemberIndexerImpl[A, R, C] = macro applyTuple_impl[A, R, C]
  }

  def applyTuple_impl[A: c.WeakTypeTag, R: c.WeakTypeTag, C: c.WeakTypeTag]
    (c: blackbox.Context)(conversionsTo: c.Expr[C]) = {

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
      // c.info(NoPosition, s"mapping $typeA arities=${arities.mkString(",")}", force = true)
      val arr = arities.foldLeft(0 -> Array.empty[Int]) { (acc, e) =>  (e + acc._1, acc._2 :+ (e + acc._1)) }._2
      (0 +: arr).sliding(2).toArray.zipWithIndex.map(x => (x._2, x._1(0), x._1(1)))
    }

    val mis = types.map(t => create(c)(t, typeR, typeC, conversionsTo))

    val cases = offsets.map { case(index, from, to) =>
      val mbrName = TermName("_" + (index + 1))
      cq""" n if n >= $from && n < $to => mis.$mbrName.read(value.$mbrName, n - $from) """
    } :+ cq""" _ => throw new Exception ("index " + index + " out of range") """

    q"""
       new org.phasanix.memidx.MemberIndexerImpl[$typeA, $typeR, $typeC]($names, $conversionsTo)  {
          val mis = (..$mis)

         def read(value: $typeA, index: Int): $typeR = {
           index match {
             case ..$cases
           }
         }
       }
     """
  }

  def apply_impl[A: c.WeakTypeTag, R: c.WeakTypeTag, C: c.WeakTypeTag]
    (c: blackbox.Context)(conversionsTo: c.Expr[C]) = {
    import c.universe._
    val typeA = weakTypeOf[A]
    create[A, R, C](c)(c.weakTypeOf[A], c.weakTypeOf[R], c.weakTypeOf[C], conversionsTo)
  }

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
      } else if (returnType <:< c.universe.typeOf[Option[_]]) {
        true
      }else if (returnType.typeArgs.nonEmpty) {
        // Not attempting to handle container types other than Option
        false
      } else if (isTuple) {
        name.startsWith("_")
      } else {
        true
      }
    }.toList
  }

  // Create a MemberIndexerImpl, either for direct use by create_impl, or as part of a tuple
  private def create[A,R,C](c: blackbox.Context)
                           (ta: c.Type, tr: c.Type, tc: c.Type, conversionsTo: c.Expr[C]) = {

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
        // User-specified type: vs must have an method for it named "from<typeName>"
        val nme = TermName(s"from${returnType.typeSymbol.name}")
        c.info(NoPosition, s"mapping to conversionsTo.$nme(value: $returnType) (which must exist)\n" + "" +
          "If compiler warns about structural types, consider named instead of anonymous class", force = true)
        q"conversionsTo.$nme($tree)"
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
       new org.phasanix.memidx.MemberIndexerImpl[$ta,$tr,$tc](Seq(..$names), $conversionsTo) {
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