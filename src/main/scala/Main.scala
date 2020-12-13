package com.mkobiers.med

import algo.Eclat
import domain.{FileNotAccessible, MinConfidence, MinSupport, Rule}
import io.Reader

import java.io.File
import scala.util.Try

object Main extends App {

  val errorMapper: Throwable => Int = {
    case _: ArrayIndexOutOfBoundsException =>
      println(
        s"provided arguments are wrong or incomplete: ${args.mkString(", ")}"
      )
      1
    case FileNotAccessible =>
      println(s"file ${args(0)} is inaccessible")
      1
    case t =>
      println(s"error ${t.getClass.getSimpleName}")
      1
  }

  val resultMapper: Vector[Rule] => Int = {
    case rules if rules.nonEmpty =>
      println(s"association rules found")
      println(rules.map(rule => Rule.text(rule)).mkString("\n"))
      0
    case _ =>
      println("no association rules found")
      0
  }

  val result = for {
    input         <- Try(new File(args(0))).toEither
    minSupport    <- Try(MinSupport(args(1).toInt)).toEither
    minConfidence <- Try(MinConfidence(BigDecimal(args(2)))).toEither
    txs           <- Reader.transactions(input)
  } yield Eclat.associationRules(txs, minSupport, minConfidence)

  sys.exit(result.fold(errorMapper, resultMapper))
}
