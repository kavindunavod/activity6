object activity6_2 {

  // Function to validate input
  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      (false, Some("Name cannot be empty."))
    } else if (marks < 0) {
      (false, Some("Marks cannot be negative."))
    } else if (totalMarks <= 0) {
      (false, Some("Total possible marks must be a positive number."))
    } else if (marks > totalMarks) {
      (false, Some("Marks cannot exceed total possible marks."))
    } else {
      (true, None)
    }
  }

  // Function to read student info
  def getStudentInfo(): (String, Int, Int, Double, Char) = {
    println("Enter student's name:")
    val name = scala.io.StdIn.readLine()

    println("Enter student's marks:")
    val marks = scala.io.StdIn.readInt()

    println("Enter total possible marks:")
    val totalMarks = scala.io.StdIn.readInt()

    val (isValid, errorMessage) = validateInput(name, marks, totalMarks)

    if (!isValid) {
      println(errorMessage.getOrElse("Invalid input."))
      return getStudentInfo()  // Retry if input is invalid
    }

    val percentage = (marks.toDouble / totalMarks) * 100
    val grade = percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _ => 'D'
    }

    (name, marks, totalMarks, percentage, grade)
  }

  // Function to print student record
  def printStudentRecord(studentRecord: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = studentRecord

    println(s"Student Name: $name")
    println(s"Marks: $marks")
    println(s"Total Possible Marks: $totalMarks")
    println(f"Percentage: $percentage%.2f%%")
    println(s"Grade: $grade")
    println("-----------")
  }

  // Function to get student info with retry
  def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    var isValid = false
    var studentRecord: (String, Int, Int, Double, Char) = ("", 0, 0, 0.0, 'D')

    while (!isValid) {
      studentRecord = getStudentInfo()
      val (name, marks, totalMarks, percentage, grade) = studentRecord
      val validation = validateInput(name, marks, totalMarks)

      if (validation._1) {
        isValid = true
      } else {
        println(validation._2.getOrElse("Invalid input. Please try again."))
      }
    }
    studentRecord
  }

  // Function to get number of students and their records
  def getAllStudentRecords(numStudents: Int): List[(String, Int, Int, Double, Char)] = {
    var studentRecords: List[(String, Int, Int, Double, Char)] = List()

    for (_ <- 1 to numStudents) {
      println(s"Enter details for student ${studentRecords.size + 1}:")
      val studentRecord = getStudentInfoWithRetry()
      studentRecords = studentRecords :+ studentRecord
    }

    studentRecords
  }

  def main(args: Array[String]): Unit = {
    println("Enter the number of students:")
    val numStudents = scala.io.StdIn.readInt()

    val studentRecords = getAllStudentRecords(numStudents)
    println("\nStudent Records:")
    println("---------------")

    studentRecords.foreach(printStudentRecord)
  }
}
