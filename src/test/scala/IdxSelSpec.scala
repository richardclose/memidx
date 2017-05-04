import org.phasanix.memidx.{BaseConversionsTo, ConversionsTo, ConversionsToString, MemberIndexer}
import org.scalatest._
import org.w3c.dom.{Document, Element}

case class Foo(a: Int, b: Double, s: String) {
  def calculated(): String = s + s
}

case class Bar[A,B](a: A, b: B)

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

    val ir = MemberIndexer.create[Foo, String](convToStr)

    val foo = Foo(11, 14.4, "blah")

    ir.read(foo, 0) shouldBe "11"
    ir.read(foo, 1) shouldBe "14.4"
    ir.read(foo, 2) shouldBe "blah"
    ir.read(foo, 3) shouldBe "blahblah"
  }

  it should "read members of parameterized case class" in {
    val ir = MemberIndexer.create[Bar[Boolean, java.util.Date], String](convToStr)

    val bar = Bar(false, new java.util.Date(10000L))

    ir.read(bar, 0) shouldBe "false"
  }

  it should "read members of tuple" in {

    val ir = MemberIndexer.create[(Double, Long), String](convToStr)

    val tup = (2.2, 40L)

    ir.read(tup, 0) shouldBe "2.2"
    ir.read(tup, 1) shouldBe "40"
  }

  it should "read overloaded of user defined type" in {
    val doc = javax.xml.parsers.DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument()

    val ir = MemberIndexer.create[Bar[Double, Symbol], Element](new ConvToXml(doc))
    val bar = Bar(3.3, 'Thing)
    ir.read(bar, 1).getTextContent shouldBe "Thing"
  }

  it should "handle Option members" in {
    val ir = MemberIndexer.create[Bar[Option[Boolean], Option[String]], String](convToStr)
    val bar = Bar(None: Option[Boolean], Option("wibble"))
    ir.read(bar, 0) shouldBe convToStr.nilValue
    ir.read(bar, 1) shouldBe "wibble"
  }

  it should "read members by name" in {
    val ir = MemberIndexer.create[Foo, String](convToStr)

    val foo = Foo(11, 14.4, "blah")

    ir.read(foo, "s").get shouldBe "blah"
    ir.read(foo, "wibble").isEmpty shouldBe true
    ir.read(foo, "calculated").get shouldBe "blahblah"
  }

}
