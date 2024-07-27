object Activity6_2 {

  // Function to validate input
  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty) (false, Some("Name cannot be empty."))
    else if (marks < 0) (false, Some("Marks cannot be negative."))
    else if (totalMarks <= 0) (false, Some("Total possible marks must be a positive number."))
    else if (marks > totalMarks) (false, Some("Marks cannot exceed total possible marks."))
    else (true, None)
  }

  // Function to read student info
  def getStudentInfo(): (String, Int, Int, Double, Char) = {
    println("Enter student's name:")
    val name = scala.io.StdIn.readLine()

    println("Enter student's marks:")
    val marks = scala.io.StdIn.readInt()

    println("Enter total possible marks:")
    val totalMarks = scala.io.StdIn.readInt()

    validateInput(name, marks, totalMarks) match {
      case (true, _) =>
        val percentage = (marks.toDouble / totalMarks) * 100
        val grade = percentage match {
          case p if p >= 90 => 'A'
          case p if p >= 75 => 'B'
          case p if p >= 50 => 'C'
          case _ => 'D'
        }
        (name, marks, totalMarks, percentage, grade)
      case (_, Some(errorMessage)) =>
        println(errorMessage)
        getStudentInfo() // Retry if input is invalid
    }
  }

  // Function to print student record
  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println(s"Student Name: $name")
    println(s"Marks: $marks")
    println(s"Total Possible Marks: $totalMarks")
    println(f"Percentage: $percentage%.2f%%")
    println(s"Grade: $grade")
    println("-----------")
  }

  // Function to get number of students and their records
  def getAllStudentRecords(numStudents: Int): List[(String, Int, Int, Double, Char)] = {
    (1 to numStudents).toList.map { i =>
      println(s"Enter details for student $i:")
      getStudentInfo()
    }
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
