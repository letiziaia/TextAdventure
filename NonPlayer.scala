package o1.adventure

import scala.collection.mutable.Map
import scala.collection.mutable.Buffer

class NonPlayer (val name: String, val startingArea: Area, private val inventory: Buffer[Item]) {
  
  private var currentLocation = startingArea        
  
  /** NPC inventory */
  val inventoryList = Map[String, Item]()
    for (elm <- inventory){
      inventoryList += elm.name -> elm
    }
  
  
  /** Returns the current location of the non-player. */
  def location = this.currentLocation  
  
  
  /** NPCs can get items from the player and eventually give something to the player */
  def receive(player: Player, itemName: String): String = {
    if (player.has(itemName)) {
      this.inventoryList += itemName -> player.inventoryList(itemName)
      this.startingArea.npcList(this) += player.inventoryList(itemName)
      player.inventoryList.remove(itemName)
      if (itemName == "jaloviina") {
        player.inventoryList += "overall patch" -> new Item("overall patch", "It's a brand new patch. It says \"Akateeminen Jaloviinasankari\"")
        ("Thank you, this jaloviina bottle will save our next sit-sit! Let me thank you with a gift!\n") +
        ("Check your inventory.")
      } else if (itemName == "paper") {
        player.inventoryList += "cs-a1110" -> new Item("cs-a1110", "Completed course: \"Programming 1\"")
        ("Thank you, this Scala docs is really useful! Since you understand the value of documentation, you passed this course!\n") +
        ("Check your inventory.")
      } else if (itemName == "notebook") {
        player.inventoryList += "ms-a0111" -> new Item("ms-a0111", "Completed course: \"Differential and Integral Calculus 1\"")
        ("Oh, this notebook has plenty of exercises! Now I can show you how to solve differential equations!\n") +
        ("[...]\n") +
        ("You didn't fall asleep while we were doing math, did you?\n") +
        ("Check your inventory.")
      } else {(this.name + " picks up the " + itemName + ".")}
    } else 
      ("You can't give " + itemName + " to " + this.name + ".")
  }
  
  
  /** Checks if the NPC has the item in the inventory */
  def has(itemName: String): Boolean = {
    this.inventoryList.contains(itemName)
  }     
  
  
  /** Description of the NPC, where (s)he is and his/her inventory */
  override def toString = {
    val whoWhere = this.name + " Teekkari waits in " + this.currentLocation.name 
    var what = ""
    if (!this.inventoryList.isEmpty) {
      what = " with " + this.inventoryList.keys.mkString(", ") + "\n"
    } else {
      what = "\n"
    }
    whoWhere + what
  }
  
}