package net.mcribbs.s8008

case class Memory(data: Array[Byte] = new Array[Byte](Memory.MAX_MEMORY)) {
  
  //TODO can I enforce at compile time?
  //https://stackoverflow.com/questions/69183363/scala-enforce-length-of-array-collection-parameter
  require(data.length == Memory.MAX_MEMORY, s"Max supported memory is ${Memory.MAX_MEMORY/1000}Kb")

  def writeByte(address: Short, value: Byte): Memory = {
    // Ummm, pretending to be immutable with the mutable Array... Ok????
    val newData = data.clone()
    newData(address) = value
    Memory(newData)
  }
  def readByte(address: Short): Byte = data(address)
  def readAddress(address: Short): Short = ((data(address+1) << 8) + data(address)).toShort

  def logBytes(startAddress: Short, length: Int): Unit = {
    println(f"Memory dump - start address: $startAddress%#06x Bytes: $length")
    for (i <- 0 until length)
      print(f"${data(i)}%02x ")
      if (i+1) % 8 == 0 then println("")

    println()
    println()
  }
}

case object Memory:
  val MAX_MEMORY = 16000
