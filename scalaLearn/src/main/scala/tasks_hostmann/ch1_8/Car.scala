package tasks_hostmann.ch1_8

/**
  * Created by ilnur on 17.11.16.
  */
class Car(val manufacturer: String, val model: String) {
  private var year: Int    = -1
  var regNum:       String = ""
  def this(manufacturer: String, model: String, year: Int) {
    this(manufacturer, model)
    this.year = year
  }
  def this(manufacturer: String, model: String, regNum: String) {
    this(manufacturer, model)
    this.regNum = regNum
  }
  def this(manufacturer: String, model: String, regNum: String, year: Int) {
    this(manufacturer, model, regNum)
    this.year = year
  }
  def years = year
}
