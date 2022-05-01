object Main {

  def test(firstNumber: Double, secondNumber: Double): Double = {
    if (firstNumber == secondNumber) (firstNumber + secondNumber) * 3
    else firstNumber + secondNumber
  }
  def test(firstInteger: Int, secondInteger: Int): Int = {
    if (firstInteger == secondInteger) (firstInteger + secondInteger) * 3
    else firstInteger + secondInteger
  }

  def main(args: Array[String]): Unit = {
    println("Result: " + test(1, 2));
    println("Result: " + test(3, 3));
    println("Result: " + test(1.0, 2));
    println("Result: " + test(1.0, 3.0));

    var forLoopInteger = 0;

    for (forLoopInteger <- 1 to 10) {
      println("Value of forLoopInteger: " + forLoopInteger);
    }

  }
}
