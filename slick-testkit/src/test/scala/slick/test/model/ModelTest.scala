package slick.test.model

import org.junit.Test
import slick.model._

/** Test case for the SQL schema support in table definitions */
class ModelTest {

  @Test def testConsistencyCheck: Unit = {
    Model(Seq()).assertConsistency

    val A = QualifiedName("A")
    val B = QualifiedName("B")
    val a_id = Column("id",A,"Int",false,Set())
    val b_id = Column("id",B,"Int",false,Set())
    val b_a_id = Column("a_id",B,"Int",false,Set())

    // avoid failing on duplicate foreign key None names (e.g. happens on sqlite)
    Model(Seq(
      Table(B,Seq(
        b_id,
        b_a_id
      ),None,Seq(
        ForeignKey(None,B,Seq(b_a_id),A,Seq(a_id),ForeignKeyAction.NoAction,ForeignKeyAction.NoAction),
        ForeignKey(None,B,Seq(b_a_id),A,Seq(a_id),ForeignKeyAction.NoAction,ForeignKeyAction.NoAction)
      ),Seq()),
      Table(A,Seq(a_id),None,Seq(),Seq())
    )).assertConsistency
  }
}
