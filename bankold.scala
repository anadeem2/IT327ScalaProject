object bank{
  trait accountDetails {

      var name : String
      var id : Int

      def accountDetails(): String ={
      return ("Name: " + name +
              "\nAccount number: " + id)
      }

      def setName(name : String): Unit ={
        // Check no numbers in name
        if(!(name.matches(".*\\d+.*")))

          this.name = name
        else
          this.name = "No Name"
      }

  }

  abstract class Account(){

    def deposite(amount:Double) : Unit
    def withdraw(amount:Double) : Boolean

  }

  object accountNumber {
    private var accountNumber = 0
    def newAccountNumber(): Int = { 
      accountNumber +=1
      return accountNumber
      }
  }

  class Credit(accName:String) extends Account with accountDetails{
    var name = ""
    this.setName(accName)
    var id = accountNumber.newAccountNumber()
    var balance = 0.0
    var credit = 0.0
    this.setCredit(credit)
    

    def setCredit(creditLimit:Double): Unit = {
        this.credit = creditLimit
    }

    def getCredit(): Double = {
        return this.credit
    }

    override def withdraw(amount:Double): Boolean = {
      if(credit > amount){
        this.setCredit(credit-amount)
        balance += amount
        return true
      }
      else{
        return false
      }
    }

    override def deposite(amount:Double): Unit = {
      this.balance -= amount
    }


    def getBalance():Double ={
      return this.balance
    }

    def computeInterest(years:Int):Double ={
      if(years == 0){
        return this.balance * .35
      }
      return .35 * (this.balance + computeInterest(years-1))
    }

    def applyInterest(years:Int): Unit ={
        this.balance += computeInterest(years)
    }


  }

  def main(args: Array[String]): Unit ={


  val creditCard = new Credit("Awais")
  creditCard.setCredit(1000.0)

  println("Opened credit card:\n" + creditCard.accountDetails())
  println("Available credit: " + creditCard.getCredit())
  println("Current balance: " + creditCard.getBalance())
  println("Buy ps5 for $883.60")
  creditCard.withdraw(883.60)
  println("Available credit left: " + creditCard.getCredit())
  println("Balance due:" + creditCard.getBalance())
  println("Don't pay for 5 years: )")
  creditCard.applyInterest(5)
  println("Balance due after 5 years: " + creditCard.getBalance())
  println("Pay $500")
  creditCard.deposite(500.0)
  println("Remaining balance: " + creditCard.getBalance())
  println("Good bye world ")
  }
}