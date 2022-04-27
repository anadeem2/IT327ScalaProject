// Author: Awais Nadeem
// IT327 Scala Showcase Program4 - Hard
// Linkedlist program implementation from scratch illustrating data strcutures, memory management, 
// exceptions, classes, composition, polymorphism, pointers, OOP, and higher order functional programming


// Imports in scala
// import scala.collection.mutable.LinkedList
// import scala.concurrent.Future



// Custom created execeptions
object emptyExeception {
  class removeFromEmptyList extends RuntimeException
  class removeNonExistingElement extends RuntimeException
}

// Case class makes it so we don't need to use new to create instances
// T is generic - Parametric Polymorphism
case class MyLinkedList[T]() {
    import emptyExeception._
    // Instance variables
    // Head is a pointer/refernce
    var head: Node[T] = null
    // type inference
    var size = 0;

    // Add element to end of list
    def append(data:T): Unit = {
        // pattern matching
        head match {
            case null => {
                head = new Node(data, head)
                size += 1
            }

            // wild card matching
            case _ => {
                // reference copy since head is object reference
                var temp = head
                while(temp.next != null){
                    temp = temp.next
                }
                temp.next = new Node(data, null)
                size += 1
            }
        }
    }

    // Add element to front of list
    def push(data:T): Unit = {
        head = new Node[T](data,head)
        size += 1
    }

    // Remove element from front of list
    def pop(): T = {
        if(head == null){
            // exception throwing
            throw new removeFromEmptyList
        }
        size -= 1;
        var returnValue = head.data
        head = head.next
        return returnValue
    }

    def length(): Int = {
        return this.size
    }

    // print list
    def printList(): Unit = {

        var temp: Node[T] = head;

        if(temp == null) println("")

        else{
            while (temp.next != null) {

                print(temp.data);
                print(" ")
                temp = temp.next;
            }
        
            println(temp.data);
        }
    }  

    // Try to find data/element and remove it
    def remove(element: T): Boolean = {
        // Two pointers
        var lag: Node[T] = head
        var temp: Node[T] = head
        var found: Boolean = false

        // while loop and negation
        while (temp != null && !found) {
            // Supports identical java.equals
            if (temp.data.equals(element)) {
                // found at base
                if (temp.equals(lag)) {
                    head = temp.next
                }
                else {
                    lag.next = temp.next;
                    size -= 1
                    found = true
                }
            } else { // not found move forward
                lag = temp;
                temp = temp.next;
            }
        }
        // Either list is empty or element specific doesn't exist
        if (!found) throw new removeNonExistingElement
        return found
    }

    // update element at index specified with data element
    def update(index: Int, element: T): Boolean = {

        var temp: Node[T] = head
        // Scala does not let you modify/reassign parameters
        var currentIndex = index


        while (temp != null && currentIndex > 0) {
            temp = temp.next;
            currentIndex -= 1;
        }
        if(temp == null){
            return false
        }

        temp.data = element
        return true
    }

    // Add node at specified index with given data
    def insert(index: Int, element: T): Boolean = {

        var temp: Node[T] = head

        while (temp != null && index > 1) {
            temp = temp.next;
        }
        if(temp == null){
            return false
        }

        var newNode = new Node[T](element,temp.next)
        size +=1
        temp.next = newNode

        return true
    }

    // Find and return the data at specified index
    def elementAtIndex(index: Int): T = {

        var temp: Node[T] = head
        // Scala supports implicit styping but like ML is strongly typed
        // Need current index b/c scala does not allow modifying parameter index
        var currentIndex = 0

        // check if index is in range
        if (index > size){
            // optional semicolons - Can embed java code seemlessly inside scala
            throw new IndexOutOfBoundsException("No element at specified index (out of bounds)");
        }

        while (temp != null && index > currentIndex) {
            temp = temp.next;
            currentIndex +=1;
        }

        return temp.data 
        }

        // private method used by sort - returns node at specified index - shows Scala visibility
        private def getNode(index: Int): Node[T] = {
            var temp = head
            var currentIndex = 0

            while (currentIndex < index) {
                temp = temp.next
                currentIndex += 1
            }
            return temp
        }

    // Sort method that sorts the list given a function literal - Higher order function
    def sort(compare: (value1:T, value2:T) => Boolean): Unit =
    {
        var currentNode: Node[T] = head
        var nextNode: Node[T] = currentNode.next
        var swap: T = head.data

        // bubble sort
        if(currentNode == null) return
        else {
            while(currentNode != null) {
                nextNode = currentNode.next
                while(nextNode != null) {
                    // use passed in function to compare
                    if (compare(currentNode.data, nextNode.data)) {
                        swap = currentNode.data;
                        currentNode.data = nextNode.data;
                        nextNode.data = swap;
                    }
                    nextNode = nextNode.next
                }
                currentNode = currentNode.next
            }
        }

    }

// Super compact easiy constructure in defination for node class showing Scala's ability to make objects easily on the fly
// Sealed means the node objects can only exist in LinkedList (Has a composition manadatory enforcement - Node cannot exist without a linkedlist)
    sealed case class Node[T](var data: T, var next: Node[T]) {}

}



// Driver to showcase linked list class in action
def main(args: Array[String]): Unit ={
    import emptyExeception._

    // Create list - Implicit typing
    val myList = MyLinkedList[Int]()
    myList.append(4)
    println("Created list and added 4")
    myList.printList()
    println("Add 3 to end of list")
    myList.append(3)
    myList.printList()
    println("Add 9 to front of list")
    myList.push(9)
    myList.printList()
    println("Insert at index 1 value 30")
    myList.insert(1, 30)
    myList.printList()
    println("Update value at index 3 to 18")
    myList.update(3, 18)
    myList.printList()
    println("Remove value 4")
    myList.remove(4)
    myList.printList()
    println("Check value at index 1: " + myList.elementAtIndex(1))
    println("Pop element at front of list\nReturned element: " + myList.pop())
    myList.printList()
    println("Size of list: " + myList.length())
    println("Sort list using < operator function")

    // Define sort function and pass to higher order linked list sort method
    var sortFunction = (a:Int, b:Int) => a > b
    myList.sort(sortFunction)
    myList.printList()
    println("Add random element to list 10 times and sort")
    // Use scala util random library to generate random numbers
    val r = scala.util.Random
    // Scala for loop example
    for( _ <- 0 to 9){
        myList.push(r.nextInt(100))
    }
    println("Unsorted list")
    myList.printList()
    println("Size of list: " + myList.length())
    myList.sort(sortFunction)
    println("Sorted list")
    myList.printList()
    println("Modify sort function to make decending")
    // Redefine sort method
    sortFunction = (a:Int, b:Int) => a < b
    myList.sort(sortFunction)
    myList.printList()
    println("Try removing non-existent element -999")
    
    try{
        myList.remove(-999)
    } catch{
        case exception: removeNonExistingElement => println("Runtime error removeNonExistingElement thrown and caught now clear list")
    }
    try{
        while(true) myList.pop()
    } catch{
        case exception: removeFromEmptyList => println("List cleared and remove from empty exeception thrown\nFinal List: ")
    }
    finally{
        myList.printList()
        println("Don't need to worry about memory allocation & free as Scala supports garbage collection")
    }

    // Scala Collections List comparison - Cannot run as LinkedList are deprecated in scala

    // val collectionsList = LinkedList[Int]()
    // collectionsList.append(4)
    // println("Created list and added 4")
    // println(collectionsList)
    // println("Add 3 to end of list")
    // collectionsList.append(3)
    // println(collectionsList)
    // println("Size of list: " + collectionsList.length())
    // println("Sort collectionsList")
    // collectionsList.sort((a:Int, b:Int) => a < b)
    // println(collectionsList)
    // println("Sort collectionsList descending")
    // collectionsList.sort((a:Int, b:Int) => a > b)
    // println(collectionsList)



}