object activity6_1 {

  // Function to display products in the inventory
  def displayProduct(products: Map[Int, (String, Int, Double)]): Unit = {
    for ((productId, (name, quantity, price)) <- products) {
      println(s"Product ID: $productId, Name: $name, Quantity: $quantity, Price: $price")
    }
  }

  def main(args: Array[String]): Unit = {
    // Inventory 1
    val inventory1 = Map(
      101 -> ("Soap", 10, 150.0),
      102 -> ("Pencil", 100, 20.0),
      103 -> ("Pen", 50, 60.0),
      104 -> ("Cutter", 2, 45.0)
    )

    // Inventory 2
    val inventory2 = Map(
      102 -> ("pencil", 5, 600.0),
      104 -> ("cutter", 8, 150.0),
      105 -> ("Smartwatch", 4, 200.0)
    )

    // Display products in inventory1
    println("Inventory 1:")
    displayProduct(inventory1)

    var total=inventory1.keys.size
    println("total products in inventory1 : " + total)

    // Calculate the total value of all products in inventory1
    val totalValue: Double = inventory1.values.map { case (_, quantity, price) => quantity * price }.sum
    println(s"\nTotal Value of Products in Inventory 1: $totalValue")

    // Check if inventory1 is empty
    if (inventory1.isEmpty) {
      println("Inventory 1 is empty")
    } else {
      println("Inventory 1 is not empty")
    }

    // Merge inventory1 and inventory2, updating quantities and retaining the highest price
    val mergedInventory: Map[Int, (String, Int, Double)] = inventory1 ++ inventory2.map {
      case (id, (name, quantity, price)) =>
        inventory1.get(id) match {
          case Some((name1, quantity1, price1)) =>
            // Update quantity and retain the highest price
            id -> (name1, quantity1 + quantity, price1 max price)
          case None =>
            id -> (name, quantity, price)
        }
    }

    println("\nMerged Inventory:")
    displayProduct(mergedInventory)

    // Check if a product with a specific ID (e.g., 102) exists in inventory1 and print its details
    val productIdToCheck = 102
    inventory1.get(productIdToCheck) match {
      case Some((name, quantity, price)) =>
        println(s"\nProduct with ID $productIdToCheck exists in Inventory 1: Name: $name, Quantity: $quantity, Price: $price")
      case None =>
        println(s"\nProduct with ID $productIdToCheck does not exist in Inventory 1.")
    }
  }
}
