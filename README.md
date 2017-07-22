# memidx: address members of class or tuple by index.

## Purpose

`MemberIndexer` lets you treat the members of class as elements of an
array, so that you can render a list of objects as a grid with one 
object per row. It provides a typesafe means for reading members of a 
class or tuple by ordinal index, and converting them to a common type.
The user of `MemberIndexer` controls how types are converted by 
providing a suitable implementation of `ConversionsTo`. Public methods 
resembling property getters, i.e. those with no arguments and a return
type other than `Unit`, and which are not compiler-generated members 
of case classes or tuples such as `productIterator`, are mapped. There
is special-case handing of Options, such that `Some(x)` is converted
the same way as `x`, and `None` is converted to `ConversionsTo.nilValue`.

A tuple of objects may be indexed the same way: the index values are 
as they would be if the objects were concatenated. The names of the 
properties have "_0", "_1" etc appended to them to ensure uniqueness.

An instance of `Config` may be supplied, in order to control which
methods are enumerated by `members`, and in what order, and to provide
additional methods, or override the generated mapping of existing
methods.

The use case for `MemberIndexer` was a requirement to present case
classes as HTML in a generic way.

## Examples

```scala
import org.phasanix.memidx._

case class Label(name: String)

object Conv extends BaseConversionsTo[String]("-") {
  private val fmt = new java.text.DecimalFormat("#,##0.00")
  override def fromString(value: String): String = "\"" + value + "\""
  override def fromDouble(value: Double): String = fmt.format(value)
 
  // Conversion from arbitrary type, method name must match
  // type name.
  def fromLabel(value: Label): String = s"{$value}"
}

// Usage with case class
case class Car(kerbWeight: Double, modelName: String, registrationYear: Option[Int])

val mi = MemberIndexer.create[Car, String](Conv)

val cars = Car(2030.12, "Frod", None) :: Car(3220.55, "Ople", Some(2001)) :: Nil

val carTable = mi.names.mkString("\t") +: cars.map(m => mi.members(m).mkString("\t"))
println(carTable.mkString("\n"))

// Usage with tuple of case classes
case class Name(first: String, last: String)
case class Account(balance: Double, open: Boolean)

val mit = MemberIndexer.createTuple[(Name, Account), String](Conv)

val record = (Name("Bert", "User"), Account(1001.0, true))
println(s"account name: ${mit.read(record, 1)} is open: ${mit.read(record, 3)}")
mit.read(record, "first_0").foreach(firstName => println(s"firstName: $firstName"))

// Using a MemberIndexerView to override and add properties
val miName = MemberIndexer.create[Name, String](Conv).view
  .overridingByName (
    "first"   -> { n => n.first.toLowerCase },
    "initial" -> { n => n.first.take(1).toUpperCase }
  )
  .overridingByDisplayName(("last", "Surname") -> { n => n.last.toUpperCase } )
  .build()

```

## Further development
This library is complete with regard to my current requirements, so
I will probably not add much to it.

There are a few shortcomings with the ergonomics of the interface -- 
for example it would be nice to infer the return type from the type
of `ConversionsTo`. The requirement to call the macro implementation
from a separate compilation unit prevents any wrapping of the macro
implementation in the library. Similarly, the need for concrete types
to be available to the macro implementation prevents users of the 
library from wrapping it in generic code. (Perhaps a view bound
calling an implicit macro might solve this one).
