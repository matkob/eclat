package com.mkobiers.med
package domain

sealed trait ApplicationError extends Throwable

case object FileNotAccessible extends ApplicationError

case object EmptyTransactionSet extends ApplicationError
