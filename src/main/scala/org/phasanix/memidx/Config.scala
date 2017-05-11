package org.phasanix.memidx

/**
  * Gathers configuration information for configuring a MemberIndexer.
  *
  * @tparam A Target type
  * @tparam R Return type
  */
class Config[A, R] {
  import collection.mutable.ArrayBuffer

  private val indicesToShow = ArrayBuffer.empty[Int]
  private val namesToShow = ArrayBuffer.empty[String]
  private val indicesToHide = ArrayBuffer.empty[Int]
  private val namesToHide = ArrayBuffer.empty[String]
  private val overridesByName = ArrayBuffer.empty[(String, A => R)]
  private val overridesByIndex = ArrayBuffer.empty[(Int, A => R)]

  def showingNames(names: String*): this.type = {
    namesToShow.append(names: _ *)
    this
  }

  def showing(indices: Int*): this.type = {
    indicesToShow.append(indices: _ *)
    this
  }

  def overriding(overrides: (Int, A => R)*): this.type = {
    overridesByIndex.append(overrides: _ *)
    this
  }

  def overridingByName(overrides: (String, A => R)*): this.type = {
    overridesByName.append(overrides: _ *)
    this
  }

  def hiding(indices: Int*): this.type = {
    indicesToHide.append(indices: _ *)
    this
  }

  def hidingByName(names: String*): this.type = {
    namesToHide.append(names: _ *)
    this
  }

  def freeze(generatedNames: Seq[String]): (Seq[String], Map[String, Int], Map[Int, A => R], Seq[Int]) = {
    val (overrides, added) = overridesByName.partition(a => generatedNames.contains(a._1))
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

    (allNames, nameToIndex, overrideMap, offsetsShown)
  }

  /**
    * config for a child MemberIndexer: all settings are ignored as they will be used in
    * the parent.
    */
  def child(generatedNames: Seq[String]): (Seq[String], Map[String, Int], Map[Int, A => R], Seq[Int]) = {
    (generatedNames, generatedNames.zipWithIndex.toMap, Map.empty, Seq.tabulate(generatedNames.length)(i => i))
  }
}

object Config {
  /**
    * Action for no config.
    */
  def child[A,R](generatedNames: Seq[String]): (Seq[String], Map[String, Int], Map[Int, A => R], Seq[Int]) = {
    (generatedNames, generatedNames.zipWithIndex.toMap, Map.empty, Seq.tabulate(generatedNames.length)(i => i))
  }

}