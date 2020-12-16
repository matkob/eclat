package com.mkobiers.med
package algo

import domain.{Item, ItemSet, MinSupport}
import io.Reader

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import java.io.File
import scala.io.Source

class EclatTest extends Matchers with AnyWordSpecLike with BeforeAndAfterAll {

  val path = "src/test/resources"

  def readFrequentItems(file: File): Vector[ItemSet] = {
    val source = Source.fromFile(file)
    val itemSets = source
      .getLines()
      .map(line => line.split(" +").map(Item).toSet)
      .toVector
    source.close()
    itemSets
  }

  "Given a known transaction sets, Eclat" should {
    val transactions1 =
      Reader.transactions(new File(s"$path/in1.txt")).toOption.get
    val itemSets1 = readFrequentItems(new File(s"$path/out1.txt"))
    val transactions2 =
      Reader.transactions(new File(s"$path/in2.txt")).toOption.get
    val itemSets2 = readFrequentItems(new File(s"$path/out2.txt"))

    "find all frequent item sets in in1" in {
      val minSupport = MinSupport(2)
      val fItemSets  = Eclat.frequentItemSets(transactions1, minSupport)
      fItemSets.size shouldEqual itemSets1.size + 1
      assert(fItemSets.contains(Set.empty[Item]))
      itemSets1.foreach(itemSet => assert(fItemSets.contains(itemSet)))
    }

    "find all frequent item sets in in2" in {
      val minSupport = MinSupport(2)
      val fItemSets  = Eclat.frequentItemSets(transactions2, minSupport)
      fItemSets.size shouldEqual itemSets2.size + 1
      itemSets2.foreach(itemSet => assert(fItemSets.contains(itemSet)))
    }
  }
}
