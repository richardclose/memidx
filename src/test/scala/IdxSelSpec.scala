import org.phasanix.memidx._
import org.w3c.dom.{Document, Element}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

case class Foo(a: Int, b: Double, s: String) {
  def calculated(): String = s + s
}

case class Bar[A,B](a: A, b: B)

case class Baz(a: Boolean, b: Float)

class ConvToXml(doc: Document) extends BaseConversionsTo[Element](doc.createElement("nil")) {
  override def fromString(value: String): Element = {
    val elt = doc.createElement("str")
    elt.setTextContent(value)
    elt
  }

  // ConversionsTo does not have a method for Symbol; this method
  // is matched by name.
  def fromSymbol(value: Symbol): Element = {
    val elt = doc.createElement("sym")
    elt.setTextContent(value.name)
    elt
  }
}

class IdxSelSpec extends AnyFlatSpec with Matchers {

  val convToStr = new ConversionsToString()

  private val miFoo = MemberIndexer.create[Foo, String](convToStr)

  "MemberIndexer" should "read members of case class" in {

    val foo = Foo(11, 14.4, "blah")

    miFoo.read(foo, 0) shouldBe "11"
    miFoo.read(foo, 1) shouldBe "14.4"
    miFoo.read(foo, 2) shouldBe "blah"
    miFoo.read(foo, 3) shouldBe "blahblah"
  }

  it should "read members of parameterized case class" in {
    val mi = MemberIndexer.create[Bar[Boolean, java.util.Date], String](convToStr)

    val bar = Bar(false, new java.util.Date(10000L))

    mi.read(bar, 0) shouldBe "false"
  }

  it should "read members of tuple" in {

    val mi = MemberIndexer.create[(Double, Long), String](convToStr)

    val tup = (2.2, 40L)

    mi.read(tup, 0) shouldBe "2.2"
    mi.read(tup, 1) shouldBe "40"
  }

  it should "read arbitrary type, not defined in ConversionsTo" in {
    val doc = javax.xml.parsers.DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument()
    val mi = MemberIndexer.create[Bar[Double, Symbol], Element](new ConvToXml(doc))
    val bar = Bar(3.3, Symbol("Thing"))

    mi.read(bar, 1).getTextContent shouldBe "Thing"
  }

  it should "handle Option members" in {
    val mi = MemberIndexer.create[Bar[Option[Boolean], Option[String]], String](convToStr)
    val bar = Bar(None: Option[Boolean], Option("wibble"))
    mi.read(bar, 0) shouldBe convToStr.nilValue
    mi.read(bar, 1) shouldBe "wibble"
  }

  it should "read members by name" in {

    val foo = Foo(11, 14.4, "blah")

    miFoo.read(foo, "s").get shouldBe "blah"
    miFoo.read(foo, "wibble").isEmpty shouldBe true
    miFoo.read(foo, "calculated").get shouldBe "blahblah"
  }

  it should "read from tuple set" in {
    type A = (Foo, Bar[Boolean, String], Baz)
    val tuple: A = (Foo(3, 4.5, "six"), Bar(true, "yes"), Baz(false, 4.4f))
    val mit = MemberIndexer.createTuple[(Foo, Bar[Boolean, String], Baz), String](convToStr)

    mit.read(tuple, 1) shouldBe "4.5"
    mit.read(tuple, "calculated_0").get shouldBe "sixsix"
    val members = mit.members(tuple).toSeq

    members.last shouldBe "4.4"
  }

  "MemberIndexerView" should "read with overrides" in {
    val miv = miFoo.view
      .overriding(0 -> { a => s"a:'${a.a}'"})
      .overridingByName("calculated" -> { a => a.s + "!"})
      .build()

    val foo = Foo(1, 0.5, "wat")

    miv.read(foo, 0) shouldBe "a:'1'"
    miv.read(foo, "calculated").get shouldBe "wat!"
  }

  it should "apply filters, showing indices" in {
    val miv = miFoo.view
      .showing(3, 0)
      .build()

    val foo = Foo(1, 2.1, "who")

    miv.members(foo).toSeq shouldBe Seq("whowho", "1")
    miv.names shouldBe Seq("calculated", "a")
  }

  it should "apply filters, showing names" in {
    val miv = miFoo.view
      .showingNames("calculated", "a")
      .build()

    val foo = Foo(1, 2.1, "who")

    miv.members(foo).toSeq shouldBe Seq("whowho", "1")
    miv.names shouldBe Seq("calculated", "a")
  }

  it should "apply filters, hiding indices" in {

    val miv = miFoo.view
      .hiding(0, 1)
      .build()

    val foo = Foo(1, 2.1, "who")

    miv.members(foo).toSeq shouldBe Seq("who", "whowho")
    miv.names shouldBe Seq("s", "calculated")
  }

  it should "apply filters, hiding names" in {
    val miv = miFoo.view
      .hidingByName("a", "b")
      .build()

    val foo = Foo(1, 2.1, "who")

    miv.members(foo).toSeq shouldBe Seq("who", "whowho")
    miv.names shouldBe Seq("s", "calculated")
  }

  it should "add extra properties via view" in {
    val miv = miFoo.view
      .overridingByName("extra" -> {a => "wibble:" + a.s})
      .build()

    val foo = Foo(3, 4.5, "yes")

    miv.read(foo, "extra").get shouldBe "wibble:yes"
  }

  it should "set a display name" in {
    val miv = miFoo.view
      .overridingByDisplayName(("calculated" -> "WIBBLE") -> { a => a.calculated()})
      .build()

    miv.displayNames(3) shouldBe "WIBBLE"
  }

  it should "remap names shown" in {
    val miv = miFoo.view
      .showingDisplayNames("a" -> "Apple", "b" -> "Banana")
      .build()

    val miv1 = miv.selecting("b", "s")

    val foo = Foo(11, 12.0, "why")

    miv1.names shouldBe Seq("b", "s")
    miv1.read(foo, 1) shouldBe "why"
  }

}
