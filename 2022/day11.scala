//> using toolkit default

@main
def day11(): Unit = {
  val path = os.pwd / "day11Input.txt"
  solution(path, false)
  solution(path, true)
}

object Value:
  def unapply(s: String): Option[Int] = s.toIntOption

class Monkey(var items: Array[Long], val operation: (Long => Long), val test: Long, val trueTarget: Int, val falseTarget: Int) {
  def throwItem(monkeys: Array[Monkey], part2: Boolean, testProduct: Long): Unit = {
    var item = items.head
    items = items.tail
    item = operation(item)
    if !part2 then
      item = item / 3
    if item % test == 0 then
      monkeys(trueTarget).items = monkeys(trueTarget).items.appended(item % testProduct)
    else
      monkeys(falseTarget).items = monkeys(falseTarget).items.appended(item % testProduct)
  }
}

def solution(path: os.Path, part2: Boolean): Unit = {
  val lines = os.read.lines(path)
  val groups = lines.grouped(7)
  var monkeys: Array[Monkey] = Array[Monkey]()
  groups.foreach {
    (group) =>
      var items: Array[Long] = Array[Long]()
      val itemsRegex = "[0-9]+".r
      for item <- itemsRegex.findAllMatchIn(group(1)) do
        items = items.appended(item.group(0).toLong)
      val operation: (Long => Long) = group(2) match
        case "  Operation: new = old * old" => (x) => x * x
        case s"  Operation: new = old + ${Value(value)}" => (x) => x + value
        case s"  Operation: new = old * ${Value(value)}" => (x) => x * value
        case _ => ((x) => x)
      val test = group(3) match
        case s"  Test: divisible by ${Value(value)}" => value
        case _ => -1
      val trueTarget = group(4) match
        case s"    If true: throw to monkey ${Value(value)}" => value
        case _ => -1
      val falseTarget = group(5) match
        case s"    If false: throw to monkey ${Value(value)}" => value
        case _ => -1
      var monkey = Monkey(items, operation, test, trueTarget, falseTarget)
      monkeys = monkeys.appended(monkey)
  }
  var inspects: Array[Long] = monkeys.map(_ => 0)
  var rounds = 20
  if part2 then
    rounds = 10000
  val testProduct = monkeys.map((monkey) => monkey.test).product
  for (_ <- 0 until rounds) {
    for (i <- 0 until monkeys.size) {
      var monkey = monkeys(i)
        while !monkey.items.isEmpty do {
          monkey.throwItem(monkeys, part2, testProduct)
          inspects(i) += 1
        }
    }
  }
  inspects = inspects.sortWith((x, y) => x > y)
  println(inspects(0) * inspects(1))
}