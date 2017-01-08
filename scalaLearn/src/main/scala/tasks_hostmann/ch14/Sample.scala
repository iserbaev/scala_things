package tasks_hostmann.ch14

import java.io.IOException

import scala.beans.BeanProperty
import javax.persistence.Entity
import javax.persistence.Id
import javax.inject.{Inject, Named}

import com.sun.istack.internal.NotNull

import scala.annotation.{tailrec, varargs}

/**
  * Created by ilnur on 15.12.16.
  */
@deprecated
@throws[java.rmi.RemoteException] @SerialVersionUID(656465464654321654L)
@Entity class Sample [@specialized T] @Inject() (@Id @BeanProperty @NotNull id:String, @BeanProperty name: String, title:T) extends Cloneable{
  @BeanProperty @Named("pass") val password:String = ""
  @volatile var x:Integer = 0
  @transient var none:String = ""
  @throws(classOf[IOException]) def readSample(path: String):String = ""
  @varargs def setProperties(props:String*):Unit = {}
  @tailrec final def sum(xs: Seq[Int], partial: Int): Int = if (xs.isEmpty) partial else sum(xs.tail, xs.head+partial)
}