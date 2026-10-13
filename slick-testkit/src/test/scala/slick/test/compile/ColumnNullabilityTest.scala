package slick.test.compile

import com.typesafe.slick.testkit.util.ShouldNotTypecheck
import slick.jdbc.H2Profile.api._

/** The column types are in implicit scope only where the tables are defined, so the uses below rely on the
  * nullability that each column gets there. */
class ColumnNullabilityTest {
  sealed trait Priority
  case object Low extends Priority

  object ElementType {
    implicit val priorityType: BaseColumnType[Priority] = MappedColumnType.base[Priority, Int](_ => 1, _ => Low)
  }

  object WholeOptionType {
    implicit val optionPriorityType: BaseColumnType[Option[Priority]] =
      MappedColumnType.base[Option[Priority], Int](_.fold(0)(_ => 1), i => if(i == 0) None else Some(Low))
  }

  object Schema {
    import ElementType._

    class Tasks(tag: Tag) extends Table[(Int, Priority, Option[Priority], Option[Priority], Priority)](tag, "tasks") {
      def id = column[Int]("id")
      def priority = column[Priority]("priority")
      def nullablePriority = column[Option[Priority]]("nullable_priority")
      def wholeOptionPriority = {
        import WholeOptionType._
        column[Option[Priority]]("whole_option_priority")
      }
      def explicitPriority = column[Priority]("explicit_priority")(priorityType)
      def * = (id, priority, nullablePriority, wholeOptionPriority, explicitPriority)
    }

    class Categories(tag: Tag) extends Table[Int](tag, "categories") {
      def id: Rep[Int] = column[Int]("id")
      def * = id
    }

    class Items(tag: Tag) extends Table[(Int, Int)](tag, "items") {
      def id = column[Int]("id")
      def categoryId = column[Int]("category_id")
      def * = (id, categoryId)
      def category = foreignKey("category_fk", categoryId, TableQuery[Categories])(_.id)
    }
  }
  val tasks = TableQuery[Schema.Tasks]

  def nullability = tasks.map { t =>
    val priority: TypedRep.NotNull[Priority] = t.priority
    val nullablePriority: TypedRep.Nullable[Priority] = t.nullablePriority
    val wholeOptionPriority: TypedRep.NotNull[Option[Priority]] = t.wholeOptionPriority
    val explicitPriority: TypedRep.NotNull[Priority] = t.explicitPriority
    val lifted: TypedRep.Nullable[Priority] = Rep.Some(t.priority)
    (priority, nullablePriority, wholeOptionPriority, explicitPriority, lifted)
  }

  def comparisonTypes = tasks.map { t =>
    val plain: Rep[Boolean] = t.priority === Low
    val withOption: Rep[Option[Boolean]] = t.priority === Option[Priority](Low)
    val nullable: Rep[Option[Boolean]] = t.nullablePriority === Low
    val wholeOption: Rep[Boolean] = t.wholeOptionPriority === Some(Low)
    (plain, withOption, nullable, wholeOption)
  }

  def nullabilityKeptByMap: Query[TypedRep.NotNull[Priority], Priority, Seq] =
    tasks.map(_.priority).filter(_ === Low)

  def comparisonWithNone = ShouldNotTypecheck("""
      tasks.filter(_.nullablePriority === None)
    """, ".*Comparing a column with None.*")
}
