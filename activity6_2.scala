object Activity6_2 {
  def main(args: Array[String]): Unit = {
    import scala.io.StdIn._

    // Function to read and validate the student's name
    def readStudentName(): String = {
      print("Enter student's name: ")
      val name = readLine().trim
      if (name.isEmpty) {
        println("Name cannot be empty. Please enter a valid name.")
        readStudentName() // Recursive call to prompt for a valid name
      } else {
        name
      }
    }

    // Function to read and validate an integer input
    def readPositiveInt(prompt: String): Int = {
      print(prompt)
      try {
        val value = readInt()
        if (value > 0) value
        else {
          println("Input must be a positive integer. Please try again.")
          readPositiveInt(prompt) // Recursive call for valid input
        }
      } catch {
        case _: Exception =>
          println("Invalid input. Please enter a positive integer.")
          readPositiveInt(prompt) // Recursive call for valid input
      }
    }

    // Function to calculate the percentage and grade
    def calculateGrade(marks: Int, totalMarks: Int): (Double, String) = {
      val percentage = (marks.toDouble / totalMarks) * 100
      val grade = percentage match {
        case p if p >= 90 => "A"
        case p if p >= 75 => "B"
        case p if p >= 50 => "C"
        case _            => "D"
      }
      (percentage, grade)
    }

    // Main program logic
    val name = readStudentName()
    val totalMarks = readPositiveInt("Enter total possible marks: ")

    val marks = {
      var m = 0
      var count =1;
      while(m > totalMarks || count ==1) {
        m = readPositiveInt("Enter marks obtained: ")
        count=count+1
        if (m > totalMarks) {
          println(s"Marks cannot exceed total possible marks ($totalMarks). Please try again.")
        }
      }
      m
    }

    val (percentage, grade) = calculateGrade(marks, totalMarks)

    // Display the results
    println(s"\nStudent Name: $name")
    println(f"Marks Obtained: $marks/$totalMarks")
    println(f"Percentage: $percentage%.2f%%")
    println(s"Grade: $grade")
  }
}
