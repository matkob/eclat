package com.mkobiers.med
package io

import domain._

import java.io.File
import scala.io.{Codec, Source}
import scala.util.Try

object Reader {
  implicit val codec: Codec = Codec.UTF8

  def transactions(
      file: File
  ): Either[ApplicationError, Vector[Transaction]] = {
    Try {
      val source = Source.fromFile(file)
      val transactions = source
        .getLines()
        .zip(Iterator.iterate[Long](0L)(_ + 1L))
        .map { case (line, idx) =>
          Transaction(TransactionId(idx), line.split(" +").map(Item).toVector)
        }
        .toVector
      source.close()
      transactions
    }.toEither.left.map(_ => FileNotAccessible)
  }
}
