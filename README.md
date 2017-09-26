# memidx: address members of class or tuple by index.

## Purpose

Given an object, you want to iterate over its members, or read 
them by index or by name, while converting them to a common type. 
You might be:

- Rendering a list of objects as an HTML table (my original use case)
- Visually inspecting a single object
- Computing the total of the numeric members (a bit contrived, this one)

You want to do this with the minimum of boilerplate, and as you're 
using Scala, you want to be efficient and typesafe. This library 
addresses the requirement with a macro implementation that maps indices
directly to members.

## Usage

To use `MemberIndexer`, start by providing a suitable implementation
of `ConversionsTo[A]`. Usually, you will want to extend 
`BaseConversionsTo[A]`, which has default implementations that return
`nilValue`, so that you only need to implement the conversions that 
you are interested in. Type `A` might be `String`, or
`org.w3c.xml.Element`, or `scalatags.Text.all.Tag`, or whatever fits 
your use case. If this library is worth using, you'll probably have
one implementation of `ConversionsTo[A]` which you use for all types.
(You can override the conversion for specific members with 
`MemberIndexerView`). If your class has a member of a type not 
included in `ConversionsTo` (say, `Foo`), your implementation must have
a method (say, `def fromFoo(value: Foo): A`).

`MemberIndexer` will map any method that looks like a property reader --
the criterion is: any public method with no arguments, with a return
type other than `Unit`, that is not a compiler-generated member of 
case classes or tuples (e.g. `productIterator`). There is special-case 
handling of `Option`, such that `Some(x)` is converted the same way as
`x`, and `None` is converted to `ConversionsTo.nilValue`.

If type `A` is a tuple of objects, the index values are as they would 
be if the objects were concatenated. The names of the  properties have 
"_0", "_1" etc appended to them to ensure uniqueness.

`MemberIndexerView` provides control of which members are shown, and in 
what order. It also provides conversion of selected members, addition 
of members, and association of descriptive names for display.
`MemberIndexer.view` returns a builder object that creates a
`MemberIndexerView`.

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
library from wrapping it in generic code. (I think that a view bound
calling an implicit macro might solve this one).
