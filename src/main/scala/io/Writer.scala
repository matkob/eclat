package com.mkobiers.med
package io

import domain.Rule.text
import domain._

import java.io.{File, PrintWriter}
import scala.util.Try

object Writer {

  def write(
      file: File,
      rules: Vector[Rule]
  ): Either[ApplicationError, Unit] = {
    Try {
      val writer = new PrintWriter(file)
      rules.foreach(rule => writer.println(text(rule)))
      writer.close()
    }.toEither.left.map(_ => FileNotWritable)
  }
}
