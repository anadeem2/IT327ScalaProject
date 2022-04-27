
// Author: Awais Nadeem
// IT327 Scala showcase Program3 - Intermediate
// Bank modeling simulation demonstrating OOP in Scala including Inheritence (Abstract Classes + Interfaces/Traits), Instances (variables + objects)
// Method and function calls, Singleton objects, message interactions, return types, CLI

// Like Java, everything is a class/object - however Scala allows us to define singleton objects from the get-go. In this simulation since there is only 1 bank it is a singleton object
// This also demonstrates Scala's ability for prototype & delagation alongside the ability to OOP
object bank{

  // A trait is identical to a interface in Java - it supports Scala's ability to do multiple inheritence as unlike interfaces, traits can have implementation.
  trait accountDetails {

      // Account detail instance variables
      var name : String
      var id : Int

      // Final makes sure you cannot override in subclasses
      final var interest = .35

      // Implemented method that prints details
      def accountDetails(): String ={
      return ("Name: " + name +
              "\nAccount number: " + id)
      }

      // Regular setter, but showcase Scala's ability for RegEx
      def setName(name : String): Unit ={
        // Check no numbers in name - conditional logic
        if(!(name.matches(".*\\d+.*")))

          this.name = name
        else
          this.name = "No Name"
      }

  }

  // Abstract Class Account defines basic functions - Unit in scala is identical to void in Java/C/C++ 
  abstract class Account(){

    def deposite(amount:Double) : Unit
    def withdraw(amount:Double) : Boolean
    def getBalance():Double

  }


  // Singleton object that ensures no two accounts ever have the same number (almost like a autoincrement object assigner)
  object accountNumber {
    private var accountNumber = 0

    // Generate new account number for caller and return
    def newAccountNumber(): Int = { 
      accountNumber +=1
      return accountNumber
      }
  }


  // Credit concrecrete class representing credit card derived from Account abstract class and implements accountDetails trait
  class Credit(accName:String) extends Account with accountDetails{
    // My own custom defined exception illustating Scala exception handeling
    import withdrawExecptions._

    // instance variable initializations
    var name = ""
    this.setName(accName) // call trait method to setname
    var id = accountNumber.newAccountNumber() // generate account ID from singleton object
    var balance = 0.0
    var credit = 0.0
    this.setCredit(credit)
    

    def setCredit(creditLimit:Double): Unit = {
        this.credit = creditLimit
    }

    def getCredit(): Double = {
        return this.credit
    }

    // override keyword indicating method is overidden
    override def withdraw(amount:Double): Boolean = {
      // enough credit available
      if(credit > amount){
        this.setCredit(credit-amount) // set remaining credit
        balance += amount // update balance due
        return true
      }
      // requested amount > creditlimit creditcard denies
      else{
        throw new withdrawException
      }
    }

    // Pay off balance
    override def deposite(amount:Double): Unit = {
      this.balance -= amount
    }


    def getBalance():Double ={
      return this.balance
    }

    // applies intrest driver (compounded anually - I know irl we use PeRT but I wanted to illustate Scala's ability for recursion)
    def applyInterest(years:Int): Unit ={
        this.balance += computeInterest(years)
    }

    // Recursive method that computes interest on current balance of credit card
    def computeInterest(years:Int):Double ={
      if(years == 0){
        return this.balance * interest
      }
      return interest * (this.balance + computeInterest(years-1))
    }

  }

  // Debit class - sole purpose to showcase inheritence and OOP
    class Debit(accName:String) extends Account with accountDetails{
    // same as credit
    import withdrawExecptions._
    var name = ""
    this.setName(accName)
    var id = accountNumber.newAccountNumber()
    var balance = 0.0

    
    // different logic, now no such thing as credit directly dealing with balance
    override def withdraw(amount:Double): Boolean = {
      if(balance > amount){
        balance -= amount
        return true
      }
      else{
        throw new withdrawException
      }
    }

    // not paying off balance, but adding money to account
    override def deposite(amount:Double): Unit = {
      this.balance += amount
    }


    def getBalance():Double ={
      return this.balance
    }


  }

  // User class illustrate composition and polymorphism
  class User(name:String){
  import withdrawExecptions._

    var creditAccount: Credit = null
    var debitAccount: Debit = null


    def openCredit():Unit ={
      this.creditAccount = new Credit(name)
    }
    def openDebit():Unit ={
      this.debitAccount = new Debit(name)
    }

    def doTransactions(account:Account):Unit ={
        account.deposite(1000.0)
        println("Current balance: " + account.getBalance())
        println("Buy ps5 for $883.60")
        account.withdraw(883.60)
        println("Balance due:" + account.getBalance())
        println("Pay $500")
        account.deposite(500.0)
        println("Remaining balance: " + account.getBalance())
    }

  }

  // My custom created withdraw exception
  object withdrawExecptions {
      class withdrawException extends RuntimeException
  }

  def main(args: Array[String]): Unit ={
  
  import withdrawExecptions._

  var name:String = "Awais"

  if(args.size > 0)
    name = args(0)

  // Credit card demonstrations

  println("\n\nCredit Card Demo\n\n")


  val creditCard = new Credit(name + " Credit")
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

  // Try catch finally - Scala's exception handling illustration
  try{
    println("Try instant rich glitch by withdrawing 1 billion from bank")
    creditCard.withdraw(1000000000)
  }
  catch{
    case exception: withdrawException => println("Runtime error withdraw exception thrown, caught, and handeled appropriately")
  }
  finally{
  println("Dangit, the bank caught me, rip im going to jail")
  }
  println("\n\nEnd of Credit Card\n\n")

  // Debit card demonstrations

  println("\n\nDebit Card Demo\n\n")

  val debitCard = new Debit(name + " Debit")
  creditCard.deposite(1000.0)

  println("Opened debit card:\n" + debitCard.accountDetails())
  println("Current balance: " + debitCard.getBalance())
  println("Deposit $1000 ")
  debitCard.deposite(1000.0)
  println("Current balance: " + debitCard.getBalance())
  println("Buy ps5 for $883.60")
  debitCard.withdraw(883.60)
  println("Balance left:" + debitCard.getBalance())

  println("\n\nEnd of Debit Card\n\n")


  // User Demo

  println("\n\nUser Demo\n\n")

  val user = new User(name)

  println("\n\nUser Opens Credit Card\n\n")
  user.openCredit()
  user.creditAccount.setCredit(1000) // Get withdraw exception - munaul/hardcoded fix since purpose is to showcase composition + polymorphism
  println("Do transactions with credit card")
  // Output may seem a little weird (since not using credit as should, so technically you paid the credit card company when depositing without ever using any credit thus negative), again the purpose is to showcase polymorphism.
  user.doTransactions(user.creditAccount)
  println("\n\nNow switch to Debit Card\n\n")
  user.openDebit()
  println("Do transactions with debit card")
  user.doTransactions(user.debitAccount)
  println("\n\nEnd of User Demo\n\n")


  }
}