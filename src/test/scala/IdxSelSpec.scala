import org.phasanix.memidx.{BaseConversionsTo, ConversionsTo, ConversionsToString, MemberIndexer}
import org.scalatest._
import org.w3c.dom.{Document, Element}

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

  def from(value: Symbol): Element = {
    val elt = doc.createElement("sym")
    elt.setTextContent(value.name)
    elt
  }
}

class IdxSelSpec extends FlatSpec with Matchers {

  val convToStr = new ConversionsToString()

  "MemberIndexer" should "read members of case class" in {

    val mi = MemberIndexer.create[Foo, String](convToStr)

    val foo = Foo(11, 14.4, "blah")

    mi.read(foo, 0) shouldBe "11"
    mi.read(foo, 1) shouldBe "14.4"
    mi.read(foo, 2) shouldBe "blah"
    mi.read(foo, 3) shouldBe "blahblah"
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

  it should "read overloaded of user defined type" in {
    val doc = javax.xml.parsers.DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument()

    val mi = MemberIndexer.create[Bar[Double, Symbol], Element](new ConvToXml(doc))
    val bar = Bar(3.3, 'Thing)
    mi.read(bar, 1).getTextContent shouldBe "Thing"
  }

  it should "handle Option members" in {
    val mi = MemberIndexer.create[Bar[Option[Boolean], Option[String]], String](convToStr)
    val bar = Bar(None: Option[Boolean], Option("wibble"))
    mi.read(bar, 0) shouldBe convToStr.nilValue
    mi.read(bar, 1) shouldBe "wibble"
  }

  it should "read members by name" in {
    val mi = MemberIndexer.create[Foo, String](convToStr)

    val foo = Foo(11, 14.4, "blah")

    mi.read(foo, "s").get shouldBe "blah"
    mi.read(foo, "wibble").isEmpty shouldBe true
    mi.read(foo, "calculated").get shouldBe "blahblah"
  }

  it should "read from set" in {
    val tuple = (Foo(3, 4.5, "six"), Bar(true, "yes"), Baz(false, 4.4f))
    var mit = MemberIndexer.createTuple[(Foo, Bar[Boolean, String], Baz), String](convToStr)
    mit.read(tuple, 1) shouldBe "4.5"
    mit.read(tuple, "calculated_0").get shouldBe "sixsix"
    val members = mit.members(tuple).toSeq
    members.last shouldBe "4.4"
  }

}
