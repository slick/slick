package slick.test.lifted

import org.junit.Assert._
import org.junit.Test

/** Test cases for MappedTo[String] getting access to String extension methods */
class MappedToStringExtensionMethodSupportTest {

  @Test def testMappedToStringExtensionMethodSupport = {
    import slick.jdbc.H2Profile.api._

    class T(tag: Tag) extends Table[(Int, MyString, Option[MyString])](tag, Some("myschema"), "mytable") {
      def id = column[Int]("id")
      def myString = column[MyString]("myString")
      def optString = column[Option[MyString]]("optString")
      def * = (id, myString, optString)
    }

    val ts = TableQuery[T]

    val s1 = ts.map(_.myString.length).result.statements.head
    println(s1)
    assertTrue("MappedTo[String] can use String ExtensionMethod length", s1 contains """select length("myString")""")

    val s2 = ts.filter(_.myString like "foo%").result.statements.head
    println(s2)
    assertTrue("MappedTo[String] can use String ExtensionMethod like", s2 contains """where "myString" like 'foo%'""")

    val s3 = ts.map(_.myString ++ "!").result.statements.head
    println(s3)
    assertTrue("MappedTo[String] can use String ExtensionMethod ++", s3 contains """select "myString"||'!'""")

    val s4 = ts.filter(_.myString startsWith "foo").result.statements.head
    println(s4)
    assertTrue("MappedTo[String] can use String ExtensionMethod startsWith", s4 contains """where "myString" like 'foo%'""")

    val s5 = ts.filter(_.myString endsWith "foo").result.statements.head
    println(s5)
    assertTrue("MappedTo[String] can use String ExtensionMethod endsWith", s5 contains """where "myString" like '%foo'""")

    val s6 = ts.map(_.myString.toUpperCase).result.statements.head
    println(s6)
    assertTrue("MappedTo[String] can use String ExtensionMethod toUpperCase", s6 contains """select ucase("myString")""")

    val s7 = ts.map(_.myString.toLowerCase).result.statements.head
    println(s7)
    assertTrue("MappedTo[String] can use String ExtensionMethod toLowerCase", s7 contains """select lcase("myString")""")

    val s8 = ts.map(_.myString.ltrim).result.statements.head
    println(s8)
    assertTrue("MappedTo[String] can use String ExtensionMethod endsWith", s8 contains """select ltrim("myString")""")

    val s9 = ts.map(_.myString.rtrim).result.statements.head
    println(s9)
    assertTrue("MappedTo[String] can use String ExtensionMethod toUpperCase", s9 contains """select rtrim("myString")""")

    val s10 = ts.map(_.myString.trim).result.statements.head
    println(s10)
    assertTrue("MappedTo[String] can use String ExtensionMethod toLowerCase", s10 contains """select ltrim(rtrim("myString"))""")

    val s11 = ts.map(_.myString.reverseString).result.statements.head
    println(s11)
    assertTrue("MappedTo[String] can use String ExtensionMethod reverse", s11 contains """select reverse("myString")""")

    val s12 = ts.map(_.myString.substring(1)).result.statements.head
    println(s12)
    assertTrue("MappedTo[String] can use String ExtensionMethod substring start", s12 contains """select {fn substring("myString", 2)}""")

    val s13 = ts.map(_.myString.substring(1, 2)).result.statements.head
    println(s13)
    assertTrue("MappedTo[String] can use String ExtensionMethod substring start end", s13 contains """select {fn substring("myString", 2, 1)}""")

    val s14 = ts.map(_.myString.take(2)).result.statements.head
    println(s14)
    assertTrue("MappedTo[String] can use String ExtensionMethod take", s14 contains """select {fn substring("myString", 1, 2)}""")

    val s15 = ts.map(_.myString.drop(1)).result.statements.head
    println(s15)
    assertTrue("MappedTo[String] can use String ExtensionMethod drop", s15 contains """select {fn substring("myString", 2)}""")

    val s16 = ts.map(_.myString.replace("my", "your")).result.statements.head
    println(s16)
    assertTrue("MappedTo[String] can use String ExtensionMethod replace", s16 contains """select replace("myString",'my','your')""")

    val s17 = ts.map(_.myString.indexOf("my")).result.statements.head
    println(s17)
    assertTrue("MappedTo[String] can use String ExtensionMethod indexOf", s17 contains """select {fn locate('my', "myString")} - 1""")

    val s18 = ts.map(_.myString.*(2)).result.statements.head
    println(s18)
    assertTrue("MappedTo[String] can use String ExtensionMethod *", s18 contains """select repeat("myString",2)""")

    val s19 = ts.map(_.optString.length).result.statements.head
    println(s19)
    assertTrue("Option[MappedTo[String]] can use String ExtensionMethod length", s19 contains """select length("optString")""")

    val s20 = ts.filter(_.optString like "foo%").result.statements.head
    println(s20)
    assertTrue("Option[MappedTo[String]] can use String ExtensionMethod like", s20 contains """where "optString" like 'foo%'""")

    val s21 = ts.map(_.optString ++ "!").result.statements.head
    println(s21)
    assertTrue("Option[MappedTo[String]] can use String ExtensionMethod ++", s21 contains """select "optString"||'!'""")

    val s22 = ts.filter(_.optString startsWith "foo").result.statements.head
    println(s22)
    assertTrue("Option[MappedTo[String]] can use String ExtensionMethod startsWith", s22 contains """where "optString" like 'foo%'""")

    val s23 = ts.filter(_.optString endsWith "foo").result.statements.head
    println(s23)
    assertTrue("Option[MappedTo[String]] can use String ExtensionMethod endsWith", s23 contains """where "optString" like '%foo'""")

    val s24 = ts.map(_.optString.toUpperCase).result.statements.head
    println(s24)
    assertTrue("Option[MappedTo[String]] can use String ExtensionMethod toUpperCase", s24 contains """select ucase("optString")""")

    val s25 = ts.map(_.optString.toLowerCase).result.statements.head
    println(s25)
    assertTrue("Option[MappedTo[String]] can use String ExtensionMethod toLowerCase", s25 contains """select lcase("optString")""")

    val s26 = ts.map(_.optString.ltrim).result.statements.head
    println(s26)
    assertTrue("Option[MappedTo[String]] can use String ExtensionMethod endsWith", s26 contains """select ltrim("optString")""")

    val s27 = ts.map(_.optString.rtrim).result.statements.head
    println(s27)
    assertTrue("Option[MappedTo[String]] can use String ExtensionMethod toUpperCase", s27 contains """select rtrim("optString")""")

    val s28 = ts.map(_.optString.trim).result.statements.head
    println(s28)
    assertTrue("Option[MappedTo[String]] can use String ExtensionMethod toLowerCase", s28 contains """select ltrim(rtrim("optString"))""")

    val s29 = ts.map(_.optString.reverseString).result.statements.head
    println(s29)
    assertTrue("Option[MappedTo[String]] can use String ExtensionMethod reverse", s29 contains """select reverse("optString")""")

    val s30 = ts.map(_.optString.substring(1)).result.statements.head
    println(s30)
    assertTrue("Option[MappedTo[String]] can use String ExtensionMethod substring start", s30 contains """select {fn substring("optString", 2)}""")

    val s31 = ts.map(_.optString.substring(1, 2)).result.statements.head
    println(s31)
    assertTrue("Option[MappedTo[String]] can use String ExtensionMethod substring start end", s31 contains """select {fn substring("optString", 2, 1)}""")

    val s32 = ts.map(_.optString.take(2)).result.statements.head
    println(s32)
    assertTrue("Option[MappedTo[String]] can use String ExtensionMethod take", s32 contains """select {fn substring("optString", 1, 2)}""")

    val s33 = ts.map(_.optString.drop(1)).result.statements.head
    println(s33)
    assertTrue("Option[MappedTo[String]] can use String ExtensionMethod drop", s33 contains """select {fn substring("optString", 2)}""")

    val s34 = ts.map(_.optString.replace("my", "your")).result.statements.head
    println(s34)
    assertTrue("Option[MappedTo[String]] can use String ExtensionMethod replace", s34 contains """select replace("optString",'my','your')""")

    val s35 = ts.map(_.optString.indexOf("my")).result.statements.head
    println(s35)
    assertTrue("Option[MappedTo[String]] can use String ExtensionMethod indexOf", s35 contains """select {fn locate('my', "optString")} - 1""")

    val s36 = ts.map(_.optString.*(2)).result.statements.head
    println(s36)
    assertTrue("Option[MappedTo[String]] can use String ExtensionMethod *", s36 contains """select repeat("optString",2)""")

  }
}
final case class MyString(value: String) extends AnyVal with slick.lifted.MappedTo[String]