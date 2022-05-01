/*Kenneth Baity and Matt Tobeck
Intermediate Program 1
IT327 Section 002

A program that simulates the functionality of a simple gradebook
and allows for the assignment of student grades and the calculation
of the overall class average
*/

import scala.io.StdIn.readDouble
import java.util.ArrayList

/*A base class that students and instructors will inherit from*/
class Person(private var first_name: String = " ", private var last_name: String = " ", private var course: String = " ")
{
  /*Getters and setters for the following variables:
    first_name
    last_name
    course
    */
  def set_first_name(first_name: String) = {
    this.first_name = first_name;
  }

  def set_last_name(last_name: String) = {
    this.last_name = last_name;
  }

  def set_course(course: String) = {
    this.course = course;
  }

  def get_first_name(): String = {
    return first_name;
  }

  def get_last_name(): String = {
    return last_name;
  }
  
  def get_course(): String = {
    return course;
  }
}

/*A class for students*/
class Student(private var student_first_name: String = " ", private var student_last_name: String = " ", 
    private var course: String = " ", private var grade: Double = 0) extends Person(student_first_name, student_last_name, course)
{
  /*Getters and setters for grade*/
  def set_grade(grade: Double) = {
    this.grade = grade;
  }

  def get_grade(): Double = {
    return grade;
  }
}

/*A class for instructors*/
class Instructor(private var instructor_first_name: String = " ", private var instructor_last_name: String = " ",
 private var course: String = " ") extends Person(instructor_first_name, instructor_last_name, course)
{
  /*An array of student objects with a size of 30*/
  private var array_of_students = new Array[Student](30);
  /*The number of students enrolled in a class*/
  private var student_count: Int = 0; 
  /*Getter for instructor name*/
  def get_instructor_name(): String = {
    return "Professor " + super.get_first_name() + " " + super.get_last_name();
  }

  /*Adds a student to the array of students*/
  def add_student(student_obj: Student): Boolean = {
    
    array_of_students(student_count) = student_obj;
    student_count += 1;
    return true;
  }

  /*Displays student infomration*/
  def display_student(): Unit = {
    for (i <- 0 until student_count) {
      println("Student name: " + array_of_students(i).get_first_name() + " " + array_of_students(i).get_last_name() + " | " + array_of_students(i).get_course() + " | " + array_of_students(i).get_grade() + "%");
    }
  }

  /*Calculates the averahge numerocal grade of all students in the class*/
  def calc_class_average(): Double = {
    var sum = 0.0; 
    for (i <- 0 until student_count) {
      sum += array_of_students(i).get_grade();
    }

    return Math.round(((sum/student_count) * 100.0))/100.0;
  }

  /*Loops through the student array and prompts for a grade for each student. Enter a negative number to terminate.*/
  def assign_grade(): Unit = {
    var flag: Boolean = true;
    var i: Int =  0;
    while (flag) {
      println("What grade did " + array_of_students(i).get_first_name() + " " + array_of_students(i).get_last_name() + " get? Enter a negative number to terminate.");
      val grade = readDouble();
      if (grade < 0) //A negative number is entered
      {
          flag = false;
      } else{
          array_of_students(i).set_grade(grade);
          i += 1;
      }

      if (i == student_count){//all students have been looped through
        flag = false;
      }
    }
  }

  /*Returns the letter grade equivalent of the numerical average grade of the class*/
  def calc_average_letter_grade(): String = {
      var average_grade = calc_class_average();
      average_grade match {
       case average_grade if average_grade >= 90 => return "A";
       case average_grade if 80 until 90 contains average_grade=> return "B";
       case average_grade if 70 until 80 contains average_grade => return "C";
       case average_grade if 60 until 70 contains average_grade=> return "D";
       case average_grade if average_grade < 0 => return "Invalid";
       case _ => return "F";
    }
  }

}

/*Creates three students and an instructor for a class and demonstrates all program methods*/
object GradeBook {
  def main(args: Array[String]): Unit = {

    var obj = new Student();
    obj.set_first_name("Matt");
    obj.set_last_name("Tobeck");
    obj.set_course("IT168");

    var obj2 = new Student();
    obj2.set_first_name("Kenneth");
    obj2.set_last_name("Baity");
    obj2.set_course("IT168");

    var obj4 = new Student();
    obj4.set_first_name("Awais");
    obj4.set_last_name("Nadeem");
    obj4.set_course("IT168");


    var obj3 = new Instructor("Audra", "Heistand", "IT168");
    println("Instructor Name: " + obj3.get_instructor_name());


    obj3.add_student(obj2);
    obj3.add_student(obj);
    obj3.add_student(obj4);

    obj3.display_student();
    obj3.assign_grade();
    println("The class average is " + obj3.calc_class_average() + "%. The letter grade for the class average is: " + obj3.calc_average_letter_grade());
    obj3.display_student();
  }
}
