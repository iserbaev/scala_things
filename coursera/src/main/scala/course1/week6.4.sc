val m = Map("I" -> 1, "V" -> 5, "X" -> 10)
m("I")
//m("L")
m get "L"
m get "V"

def showRomandDigit(letter: String): String = m.get(letter) match {
  case Some(digit) => "keu founded"
  case None => "missing key"
}