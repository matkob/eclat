package com.mkobiers.med

import algo.Eclat
import domain.{FileNotAccessible, MinConfidence, MinSupport, Rule}
import io.{Reader, Writer}

import java.io.File
import scala.util.Try

object Main extends App {

  val errorMapper: Throwable => Int = {
    case _: ArrayIndexOutOfBoundsException | _: NumberFormatException =>
      println(
        s"provided arguments are wrong or incomplete: ${args.mkString(" ")}"
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
      println(s"${rules.size} association rules found")
      0
    case _ =>
      println("no association rules found")
      0
  }

  val result = for {
    input         <- Try(new File(args(0))).toEither
    output        <- Try(new File(args(1))).toEither
    minSupport    <- Try(MinSupport(args(2).toInt)).toEither
    minConfidence <- Try(MinConfidence(BigDecimal(args(3)))).toEither
    txs           <- Reader.transactions(input)
    rules         <- Right(Eclat.associationRules(txs, minSupport, minConfidence))
    _ <- Writer.write(
      output,
      rules
    )
  } yield rules

  sys.exit(result.fold(errorMapper, resultMapper))
}
