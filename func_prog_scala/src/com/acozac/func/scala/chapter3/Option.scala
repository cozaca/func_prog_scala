package com.acozac.func.scala.chapter3

import scala.{Option => _}

sealed trait Option[+A]  {

  def map[B](f: A => B): Option[B] = this match  {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
  def orElse[B >: A](ob: => Option[B]): Option[B] = this map(Some(_)) orElse(ob)
  def filter(f: A => Boolean): Option[A] = flatMap(a => if(f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  // def variance(xs: Seq[Double]): Option[Double] =
}

case class Employee(name: String, department: String)


object Main {
  def main(args: Array[String]): Unit = {
    val employeesByName: Map[String, Employee] =
      List(Employee("Alice", "R&D"), Employee("Bob", "Accounting")).
        map(e => (e.name, e)).toMap

    // val dept:Option[String] = employeesByName.get("Joe")
    // println(dept)

    println(Some(10).map(_ + 1))
    println(None.getOrElse())
  }
}

