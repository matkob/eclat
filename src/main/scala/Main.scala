package com.mkobiers.med

import algo.Eclat
import domain.MinSupport
import io.Reader

import java.io.File
import scala.util.Try

object Main extends App {

  val errorMapper: Throwable => Int = {
    t =>
      println(s"error ${t.getClass.getSimpleName}")
      1
  }

  val resultMapper: Any => Int = _ => 0

  val result = for {
    input <- Try(new File(args(0))).toEither
    minSupport <- Try(MinSupport(args(1).toInt)).toEither
    txs <- Reader.transactions(input)
    associationRules <- Eclat.associationRules(txs, minSupport)
  } yield associationRules

  sys.exit(result.fold(errorMapper, resultMapper))
}
