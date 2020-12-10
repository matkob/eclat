package com.mkobiers.med
package io

import domain.{Item, Transaction}
import error.FileNotAccessible

import java.io.File
import scala.io.{Codec, Source}
import scala.util.Try

object Reader {
  implicit val codec: Codec = Codec.UTF8

  def transactions(file: File): Either[FileNotAccessible, Vector[Transaction]] = {
    Try {
      val source = Source.fromFile(file)
      val transactions = source.getLines().map(line => Transaction(line.map(Item).toVector)).toVector
      source.clone()
      transactions
    }.toEither.left.map(t => FileNotAccessible(t.getMessage))
  }
}
