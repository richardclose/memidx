package org.phasanix.memidx

/**
  * Wrapper of MemberIndexer. The methods of MemberIndexerView.Builder
  * enable selection, ordering, overriding and association of
  * display names for the members of the wrapped MemberIndexer.
  */
class MemberIndexerView[A, R](
                               mi: MemberIndexer[A, R],
                               allNames: Seq[String],
                               nameToIndex: Map[String, Int],
                               overrideMap: Map[Int, (A) => R],
                               offsetsShown: Seq[Int],
                               allDisplayNames: Seq[String])
  extends MemberIndexer[A, R] {

  /***
    * Convert a member by index
    * @param value instance of A of which a member is converted
    * @param index index (after mapping) of member to convert
    * @return result of conversion.
    */
  def read(value: A, index: Int): R = {
    val mappedIdx = if (index < 0 || index >= offsetsShown.length)
      -1
    else
      offsetsShown(index)
    overrideMap
      .get(mappedIdx)
      .map(_.apply(value))
      .getOrElse {
        if (mappedIdx == -1) {
          mi.nil
        } else {
          mi.read(value, mappedIdx)
        }
      }
  }

  /**
    * Number of displayed methods/properties
    */
  def arity: Int = offsetsShown.length

  /**
    * Convert a member by name
    * @param value instance of A of which a member is converted
    * @param name name of member to convert
    * @return result of conversion.
    */
  def read(value: A, name: String): Option[R]
    = nameToIndex.get(name).map(read(value, _))

  /** member iterator */
  def members(value: A): Iterator[R] = new Iterator[R] {
    var pos: Int = 0
    def hasNext: Boolean = pos < offsetsShown.length

    def next(): R = {
      val ret = read(value, pos)
      pos += 1
      ret
    }
  }

  /**
    * Names of mapped properties that are shown
    */
  val names: Seq[String] = offsetsShown.map(allNames)

  /**
    * Display names of mapped properties that are shown
    */
  def displayNames: Seq[String] = offsetsShown.map(allDisplayNames)

  /**
    * Seq[name, description] for all names
    * @return
    */
  def allNamesAndDescriptions: Seq[(String, String)] = allNames.zip(allDisplayNames)

  /**
    * Create copy of this, with a list of names to show, in the order given.
    */
  def selecting(names: String*): MemberIndexerView[A, R] = {
    val newOffsetsShown = names.flatMap(nameToIndex.get)
    new MemberIndexerView[A,R](mi, allNames, nameToIndex, overrideMap, newOffsetsShown, allDisplayNames)
  }

  /**
    Create a view builder for this.
   */
  def view: MemberIndexerView.Builder[A, R] =
    new MemberIndexerView.Builder(this)

  /** Nil value */
  def nil: R = mi.nil
}

object MemberIndexerView {

  class Builder[A, R](mi: MemberIndexer[A, R]) {
    import collection.mutable.ArrayBuffer

    private val indicesToShow = ArrayBuffer.empty[Int]
    private val namesToShow = ArrayBuffer.empty[String]
    private val indicesToHide = ArrayBuffer.empty[Int]
    private val namesToHide = ArrayBuffer.empty[String]
    private val overridesByName = ArrayBuffer.empty[(String, A => R)]
    private val overridesByIndex = ArrayBuffer.empty[(Int, A => R)]
    private val displayNameMap = collection.mutable.Map.empty[String, String]

    /**
      * create the MemberIndexerView instance after calling
      * the configuration methods.
      */
    def build(): MemberIndexerView[A, R] = {
      val (allNames, nameToIndex, overrideMap, offsetsShown, allDisplayNames) = this.freeze(mi.names)
      new MemberIndexerView[A, R](mi, allNames, nameToIndex, overrideMap, offsetsShown, allDisplayNames)
    }

    /**
      * specify by name the members to show, and the order they will
      * be in.
      */
    def showingNames(names: String*): this.type = {
      namesToShow.append(names: _ *)
      this
    }

    /**
      * specify by index the members to show, and the order they will
      * be in.
      */
    def showing(indices: Int*): this.type = {
      indicesToShow.append(indices: _ *)
      this
    }

    /**
      * specify (by index) methods to override the methods
      * generated by the macro implementation.
      */
    def overriding(overrides: (Int, A => R)*): this.type = {
      overridesByIndex.append(overrides: _ *)
      this
    }

    /**
      * specify (by name) methods to override the methods
      * generated by the macro implementation.
      * It is also necessary to add the name to a call to
      * showingNames, showingDisplayNames.
      */
    def overridingByName(overrides: (String, A => R)*): this.type = {
      overridesByName.append(overrides: _ *)
      this
    }

    /**
      * Specify (by index) members to not show.
      */
    def hiding(indices: Int*): this.type = {
      indicesToHide.append(indices: _ *)
      this
    }

    /**
      * Specify (by name) members to not show.
      */
    def hidingByName(names: String*): this.type = {
      namesToHide.append(names: _ *)
      this
    }

    /**
      * Specify by name which members to show, also
      * the corresponding display names (e.g. for column headers)
      */
    def showingDisplayNames(names: (String, String)*): this.type = {
      names.foreach(displayNameMap += _)
      namesToShow.append(names.map(_._1): _ *)
      this
    }

    /**
      * Specify by name functions to override particular
      * @param overrides
      * @return
      */
    def overridingByDisplayName(overrides: ((String, String), A => R)*): this.type = {
      overrides.foreach(e => displayNameMap += e._1)
      overridesByName.append(overrides.map(e => e.copy(_1 = e._1._1)): _ *)
      this
    }

    private[memidx] def freeze(generatedNames: Seq[String]): (Seq[String], Map[String, Int], Map[Int, A => R], Seq[Int], Seq[String]) = {
      val (_, added) = overridesByName.partition(a => generatedNames.contains(a._1))
      val allNames = generatedNames ++ added.map(_._1)
      val nameToIndex = allNames.zipWithIndex.toMap
      val overrideMap = (overridesByIndex ++
        overridesByName.map(e => nameToIndex.getOrElse(e._1, -1) -> e._2)).toMap

      val allOffsetsToShow = if (indicesToShow.isEmpty && namesToShow.isEmpty) {
        Seq.tabulate(allNames.length)(i => i)
      } else {
        indicesToShow.toSeq ++ namesToShow.flatMap(nameToIndex.get)
      }

      val toHide = indicesToHide.toSeq ++ namesToHide.flatMap(nameToIndex.get)
      val offsetsShown = allOffsetsToShow.filter(i => !toHide.contains(i))
      val displayNames = allNames.map(n => displayNameMap.getOrElse(n, n))

      (allNames, nameToIndex, overrideMap, offsetsShown, displayNames)
    }
  }

}
