package com.mkobiers.med
package io

import domain.{Item, Transaction, TransactionId}
import error.FileNotAccessible

import java.io.File
import scala.io.{Codec, Source}
import scala.util.Try

object Reader {
  implicit val codec: Codec = Codec.UTF8

  def transactions(
      file: File
  ): Either[FileNotAccessible, Vector[Transaction]] = {
    Try {
      val source = Source.fromFile(file)
      val transactions = source
        .getLines()
        .zip(Iterator.iterate[Long](0L)(_ + 1L))
        .map { case (line, idx) =>
          Transaction(TransactionId(idx), line.map(Item).toVector)
        }
        .toVector
      source.close()
      transactions
    }.toEither.left.map(t => FileNotAccessible(t.getMessage))
  }
}
