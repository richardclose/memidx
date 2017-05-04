#memidx: address members of class or tuple by index.

##Purpose

`MemberIndexer` provides a typesafe means for reading members of a 
class or tuple by ordinal index, and converting them to a common type.
The user of `MemberIndexer` controls how types are converting by 
providing a suitable implementation of `ConversionsTo`. Public methods 
resembling property getters, i.e. those with no arguments and a return
type other than `Unit`, and which are not compiler-generated members 
of case classes or tuples such as `productIterator`, are mapped.

The use case for `MemberIndexer` was a requirement to present case classes
as HTML in a generic way.

##Example

```scala
import org.phasanix.memidx._

case class Car(kerbWeight: Double, modelName: String, registrationYear: Option[Int])

object Conv extends BaseConversionsTo[String]("-") {
  private val fmt = new java.text.DecimalFormat("#,##0.00")
  override def fromString(value: String): String = "\"" + value + "\""
  override def fromDouble(value: Double): String = fmt.format(value)
}

val mi = MemberIndexer.create[Car, String](Conv)

val cars = Car(2030.12, "Frod", None) :: Car(3220.55, "Ople", Some(2001)) :: Nil

val carTable = mi.names.mkString("\t") +: cars.map(m => mi.members(m).mkString("\t"))
println(carTable.mkString("\n"))

```