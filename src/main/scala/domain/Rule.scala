package com.mkobiers.med
package domain

case class Rule(
    predecessor: ItemSet,
    successor: ItemSet,
    support: Support,
    confidence: Confidence,
    metrics: Vector[Metric]
)
