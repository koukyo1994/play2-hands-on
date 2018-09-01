package models

import org.scalatest._
import scalikejdbc.scalatest.AutoRollback
import scalikejdbc._


class CompanySpec extends fixture.FlatSpec with Matchers with AutoRollback {
  val c = Company.syntax("c")

  behavior of "Company"

  it should "find by primary keys" in { implicit session =>
    val maybeFound = Company.find(1L)
    maybeFound.isDefined should be(true)
  }
  it should "find by where clauses" in { implicit session =>
    val maybeFound = Company.findBy(sqls.eq(c.id, 1L))
    maybeFound.isDefined should be(true)
  }
  it should "find all records" in { implicit session =>
    val allResults = Company.findAll()
    allResults.size should be >(0)
  }
  it should "count all records" in { implicit session =>
    val count = Company.countAll()
    count should be >(0L)
  }
  it should "find all by where clauses" in { implicit session =>
    val results = Company.findAllBy(sqls.eq(c.id, 1L))
    results.size should be >(0)
  }
  it should "count by where clauses" in { implicit session =>
    val count = Company.countBy(sqls.eq(c.id, 1L))
    count should be >(0L)
  }
  it should "create new record" in { implicit session =>
    val created = Company.create(name = "MyString")
    created should not be(null)
  }
  it should "save a record" in { implicit session =>
    val entity = Company.findAll().head
    // TODO modify something
    val modified = entity
    val updated = Company.save(modified)
    updated should not equal(entity)
  }
  it should "destroy a record" in { implicit session =>
    val entity = Company.findAll().head
    val deleted = Company.destroy(entity)
    deleted should be(1)
    val shouldBeNone = Company.find(1L)
    shouldBeNone.isDefined should be(false)
  }
  it should "perform batch insert" in { implicit session =>
    val entities = Company.findAll()
    entities.foreach(e => Company.destroy(e))
    val batchInserted = Company.batchInsert(entities)
    batchInserted.size should be >(0)
  }
}
