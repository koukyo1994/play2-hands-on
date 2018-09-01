package models

import scalikejdbc._

case class Company(
  id: Long,
  name: String) {

  def save()(implicit session: DBSession = Company.autoSession): Company = Company.save(this)(session)

  def destroy()(implicit session: DBSession = Company.autoSession): Int = Company.destroy(this)(session)

}


object Company extends SQLSyntaxSupport[Company] {

  override val tableName = "company"

  override val columns = Seq("id", "name")

  def apply(c: SyntaxProvider[Company])(rs: WrappedResultSet): Company = apply(c.resultName)(rs)
  def apply(c: ResultName[Company])(rs: WrappedResultSet): Company = new Company(
    id = rs.get(c.id),
    name = rs.get(c.name)
  )

  val c = Company.syntax("c")

  override val autoSession = AutoSession

  def find(id: Long)(implicit session: DBSession = autoSession): Option[Company] = {
    withSQL {
      select.from(Company as c).where.eq(c.id, id)
    }.map(Company(c.resultName)).single.apply()
  }

  def findAll()(implicit session: DBSession = autoSession): List[Company] = {
    withSQL(select.from(Company as c)).map(Company(c.resultName)).list.apply()
  }

  def countAll()(implicit session: DBSession = autoSession): Long = {
    withSQL(select(sqls.count).from(Company as c)).map(rs => rs.long(1)).single.apply().get
  }

  def findBy(where: SQLSyntax)(implicit session: DBSession = autoSession): Option[Company] = {
    withSQL {
      select.from(Company as c).where.append(where)
    }.map(Company(c.resultName)).single.apply()
  }

  def findAllBy(where: SQLSyntax)(implicit session: DBSession = autoSession): List[Company] = {
    withSQL {
      select.from(Company as c).where.append(where)
    }.map(Company(c.resultName)).list.apply()
  }

  def countBy(where: SQLSyntax)(implicit session: DBSession = autoSession): Long = {
    withSQL {
      select(sqls.count).from(Company as c).where.append(where)
    }.map(_.long(1)).single.apply().get
  }

  def create(
    name: String)(implicit session: DBSession = autoSession): Company = {
    val generatedKey = withSQL {
      insert.into(Company).namedValues(
        column.name -> name
      )
    }.updateAndReturnGeneratedKey.apply()

    Company(
      id = generatedKey,
      name = name)
  }

  def batchInsert(entities: Seq[Company])(implicit session: DBSession = autoSession): List[Int] = {
    val params: Seq[Seq[(Symbol, Any)]] = entities.map(entity =>
      Seq(
        'name -> entity.name))
    SQL("""insert into company(
      name
    ) values (
      {name}
    )""").batchByName(params: _*).apply[List]()
  }

  def save(entity: Company)(implicit session: DBSession = autoSession): Company = {
    withSQL {
      update(Company).set(
        column.id -> entity.id,
        column.name -> entity.name
      ).where.eq(column.id, entity.id)
    }.update.apply()
    entity
  }

  def destroy(entity: Company)(implicit session: DBSession = autoSession): Int = {
    withSQL { delete.from(Company).where.eq(column.id, entity.id) }.update.apply()
  }

}
