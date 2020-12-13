package com.mkobiers.med
package domain

sealed trait Metric {
  def value(): Double
}
