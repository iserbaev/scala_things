package tasks_hostmann.ch10

import java.beans.{PropertyChangeEvent, PropertyChangeListener}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by ilnur on 25.11.16.
  * ch10_tsk5
  */
trait PropertyChangeSupport {
  import scala.collection.mutable
  private var arrayBuffer = mutable.ArrayBuffer[PropertyChangeListener]()
  def addPropertyChangeListener(listener: PropertyChangeListener): Unit =
    arrayBuffer += listener
  def removePropertyChangeListener(listener: PropertyChangeListener): Unit =
    arrayBuffer -= listener
  def firePropertyChange(propertyName: String, oldValue: Any, newValue: Any) {
    if (oldValue == None || newValue == None || !oldValue.equals(newValue)) {}
  }
  def firePropertyChange(event: PropertyChangeEvent) {
    val oldValue = Option(event.getOldValue)
    val newValue = Option(event.getNewValue)
  }
  private def fire(
    listeners: ArrayBuffer[PropertyChangeListener],
    event:     PropertyChangeEvent
  ) {
    if (listeners.nonEmpty) listeners.foreach(_.propertyChange(event))
  }
}
