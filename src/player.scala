import java.io.{Reader, StringBufferInputStream, StringReader}
import java.util

import Robot.State

import math._
import scala.util._
import scala.collection.JavaConversions._
import scala.collection.immutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.language.postfixOps

object Constants {
    val isLocal = false

    val TimesNone = 10
    val MaxTime = if(isLocal)20000 else 950

    val MAP_WIDTH = 19
    val MAP_HEIGHT = 10
    val MAP_AREA: Int = MAP_WIDTH * MAP_HEIGHT
    val UP = 0
    val RIGHT = 1
    val DOWN = 2
    val LEFT = 3
    val NONE = 4
    val VOID = 5
    val DEATH_INFINITE_LOOP = 0
    val DEATH_VOID = 1

    object GA {
      val PoolSize = if(isLocal) 200 else 100
      val SelectionSize = if(isLocal) 1.0/100.0 else 1.0/100.0
      val MaxGenerations = if(isLocal) 50 else 10
      val Elitism = 1
    }

    def typeToChar(`type`: Int): Char = {
        if (`type` == UP) return 'U'
        else if (`type` == RIGHT) return 'R'
        else if (`type` == DOWN) return 'D'
        else if (`type` == LEFT) return 'L'
        else if (`type` == NONE) return '.'
        else if (`type` == VOID) return '#'
        throw new IllegalArgumentException(`type` + " is not a valid direction for dirToChar")
    }

    def charToType(c: Char): Int = {
        val c2 = Character.toLowerCase(c)
        if (c2 == 'u') return UP
        else if (c2 == 'r') return RIGHT
        else if (c2 == 'd') return DOWN
        else if (c2 == 'l') return LEFT
        else if (c2 == '.') return NONE
        else if (c2 == '#') return VOID
        throw new IllegalArgumentException(c + " is not a valid character for charToDir")
    }
}

import Constants._

object Cell {
    var globalId = 0
}


class Cell(var x: Int, var y: Int, var id: Int) {

    var nexts = Array.ofDim[Cell](4)
    var `type` = NONE

    def distance(cell: Cell): Int = Math.abs(x - cell.x) + Math.abs(y - cell.y)
}

object Robot {
    var globalId = 0

    case class State(cell: Cell, direction: Int)

}

class Robot() {
    var id = 0
    var cell: Cell = _
    var direction = 0
    var states: util.Set[State] = new util.HashSet[State]()
    var death = 0

    def registerState: Boolean = states.add(Robot.State(cell, direction))
}

import java.util



class Engine(val robots: util.ArrayList[Robot], val grid: Array[Array[Cell]]) {
    var score = 0

    val wrecks = new util.HashSet[Robot]

    def registerStates(): Unit = {
        for (robot <- robots) {
            robot.registerState
        }
    }

    def get(x: Int, y: Int): Cell = {
        var x2 = x
        var y2 = y
        if (x2 < 0) x2 += MAP_WIDTH
        else if (x2 >= MAP_WIDTH) x2 -= MAP_WIDTH
        if (y2 < 0) y2 += MAP_HEIGHT
        else if (y2 >= MAP_HEIGHT) y2 -= MAP_HEIGHT
        grid(x2)(y2)
    }

    def play(): Unit = {
        wrecks.clear()
        // Increase the score
        score += robots.size
        for (robot <- robots) { // Get the next cell
            val next = robot.cell.nexts(robot.direction)
            // Move the robot
            robot.cell = next
            // This is a void cell, RIP robot
            if (next.`type` == VOID) {
                robot.death = DEATH_VOID
                wrecks.add(robot)
            } else {
                // Change the direction of the robot if we must
                if (next.`type` != NONE) robot.direction = next.`type`
                // Register the new state and check for infinite loop
                if (!robot.registerState) {
                    robot.death = DEATH_INFINITE_LOOP
                    wrecks.add(robot)
                }
            }

        }
        // Garbage collection
        robots.removeAll(wrecks)
    }

    def playToEnd(): Unit = {
        while(robots.nonEmpty){
            this.play()
        }
    }
}

case class ArrowAction(x: Int, y: Int, dir: Int) {
  override def toString: String = s"${typeToChar(dir)}($x,$y)"
}

object Player extends App {

    type Gene = Int
    type Chromosome = Array[Array[Gene]]

    def get(grid: Array[Array[Cell]], x: Int, y: Int): Cell = {
        var x2 = x
        var y2 = y
        if (x2 < 0) x2 += MAP_WIDTH
        else if (x2 >= MAP_WIDTH) x2 -= MAP_WIDTH
        if (y2 < 0) y2 += MAP_HEIGHT
        else if (y2 >= MAP_HEIGHT) y2 -= MAP_HEIGHT
        grid(y2)(x2)
    }

    def apply(grid: Array[Array[Cell]], robots: util.ArrayList[Robot], x: Int, y: Int, direction: Int): Unit = {
        val cell = get(grid, x, y)
        cell.`type` = direction
        // Check if we need to update a robot direction
        for (robot <- robots) {
            if (robot.cell == cell) robot.direction = direction
        }
    }

    def random(pourcent: Int) = Random.nextInt(100) < pourcent
    def randomRange(from: Int, to: Int) = Random.nextInt(to - from) + from
    def randomFrom[A](values: Seq[A]): A = values(Random.nextInt(values.size))

    def getRandomChromosome(grid: Array[Array[Cell]]): Chromosome = {


        val newChromosome: Chromosome = Array.tabulate[Int](MAP_HEIGHT, MAP_WIDTH)((y:Int,x:Int) => VOID)

        grid
          .flatten
          .toList
          .foreach {
              case c: Cell if c.`type` == NONE =>
                newChromosome(c.y)(c.x) = randomGene(c)
              case c: Cell =>
                newChromosome(c.y)(c.x) = c.`type`
          }
        newChromosome
    }

  private def randomGene(cell: Cell) = {
    val nones = List.fill(TimesNone)(NONE)
    val basicDirs = UP :: DOWN :: LEFT :: RIGHT :: Nil
    randomFrom(nones ++ basicDirs.flatMap(e => if (cell.nexts(e).`type` == VOID) None else Some(e)))
  }

  def applySolution(solution: Chromosome, robots: util.ArrayList[Robot], grid: Array[Array[Cell]]): Unit =
      0 until MAP_HEIGHT foreach { y =>
        0 until MAP_WIDTH foreach { x =>
          if(grid(y)(x).`type` != NONE && solution(y)(x) != grid(y)(x).`type`){
            throw new RuntimeException(s"Tried to put ${solution(y)(x)} on ($x, $y) but it was a ${grid(y)(x).`type`}")
          } else if(solution(y)(x) != NONE) {
            apply(grid, robots, x, y, solution(y)(x))
          }
        }
      }


  def rebuildActionsFromChromosome(grid: Array[Array[Cell]], chromosome: Chromosome):  List[ArrowAction] = {
    val res = ArrayBuffer.empty[ArrowAction]
    0 until MAP_HEIGHT foreach { y =>
      0 until MAP_WIDTH foreach { x =>
        val gene = chromosome(y)(x)
        val originalGridValue = grid(y)(x).`type`
        if(gene != originalGridValue && originalGridValue != NONE){
          throw new RuntimeException(s"Tried to put $gene in ($x, $y) but it was already a $originalGridValue")
        } else if(originalGridValue != gene && gene != NONE){
          res += ArrowAction(x, y, gene)
        }
      }
    }
    res.toList
  }

  def showChromosome(c: Chromosome): String = c.map(_.map(typeToChar)).map(_.mkString("")).mkString("\t")

  def mkGridFromInput(input: Array[Array[Char]]): Array[Array[Cell]] = {
    // Initialize an empty map
    val grid = Array.tabulate[Cell](MAP_HEIGHT, MAP_WIDTH)((y:Int,x:Int) => {
      val c = new Cell(x,y, Cell.globalId)
      Cell.globalId += 1
      c
    })

    // Link cells
    0 until MAP_HEIGHT foreach { y =>
      0 until MAP_WIDTH foreach { x =>
        val cell = get(grid, x, y)
        cell.nexts(UP) = get(grid, x, y - 1)
        cell.nexts(RIGHT) = get(grid, x + 1, y)
        cell.nexts(DOWN) = get(grid, x, y + 1)
        cell.nexts(LEFT) = get(grid, x - 1, y)
      }
    }

    0 until MAP_HEIGHT foreach { y =>
      0 until MAP_WIDTH foreach { x =>
        val c = input(y)(x)
        val cell = get(grid, x, y)
        cell.`type` = charToType(c)
      }
    }

    grid
  }

  def evolve(originalGrid: Array[Array[Cell]], selected: List[Chromosome]): List[Chromosome] = {
    val res = ListBuffer.empty[Chromosome]
    while(res.size < GA.PoolSize - GA.Elitism){
      val parent1 = randomFrom(selected)
      val parent2 = randomFrom(selected)

      val newChild: Chromosome = Array.fill[Int](MAP_HEIGHT, MAP_WIDTH)(VOID)
      0 until MAP_HEIGHT foreach { y =>
        0 until MAP_WIDTH foreach { x =>
          newChild(y)(x) = if(random(98)){
            parent1(y)(x)
          } else {
            parent2(y)(x)
          }
          if(originalGrid(y)(x).`type` == NONE && random(5)) {
            newChild(y)(x) = randomFrom(LEFT :: RIGHT :: UP :: DOWN :: NONE :: Nil)
          }
        }
      }

//      Console.err.println("Result of crossover between")
//      Console.err.println(showChromosome(parent1))
//      Console.err.println("and")
//      Console.err.println(showChromosome(parent2))
//      Console.err.println("gives")
//      Console.err.println(showChromosome(newChild))
      res += newChild
    }
    res.toList
  }

  def findBestSolution(input: Array[Array[Char]], robotsInputs: Seq[String]) = {
      val start = System.currentTimeMillis()

      val startGrid = mkGridFromInput(input)

      var chromosomesScored = List.empty[(Chromosome, Int)]

      // init with random chromosomes
      chromosomesScored = (0 until GA.PoolSize).toList map { indexChromosome =>
        val chromosome = getRandomChromosome(startGrid)
        val score: Gene = scoreChromosome(input, robotsInputs, chromosome)
        (chromosome, score)
      }

      chromosomesScored = chromosomesScored.sortBy(_._2)(Ordering.Int.reverse)

      var bestScore = -1

      0 until GA.MaxGenerations foreach { generation =>

        val elitism = chromosomesScored.take(GA.Elitism).map(_._1)

        val selected:List[Chromosome] = chromosomesScored.take((GA.PoolSize * GA.SelectionSize).floor.toInt).map(_._1)

        val evolved: List[Chromosome] = evolve(startGrid, selected)

        val newPool = evolved ++ elitism

        assert(newPool.size == GA.PoolSize)

        val newPoolScored = newPool.map(c => (c, scoreChromosome(input, robotsInputs, c)))

//        Console.err.println(newPoolScored.map(c => c._2 + " with " + showChromosome(c._1)).mkString("\n"))
        chromosomesScored = newPoolScored

        chromosomesScored = chromosomesScored.sortBy(_._2)(Ordering.Int.reverse)

        val thisGenBestResult = chromosomesScored.head
        val thisGenBestScore = thisGenBestResult._2
        if(thisGenBestScore > bestScore) {
          Console.err.println(s"Gen $generation best : $thisGenBestScore with ${showChromosome(thisGenBestResult._1)}")
          bestScore = thisGenBestScore
        }
      }


      val (finalChromosome, finalScore) = chromosomesScored.head
      (rebuildActionsFromChromosome(startGrid, finalChromosome), finalScore)
    }

  private def scoreChromosome(input: Array[Array[Char]], robotsInputs: Seq[String], chromosome: Chromosome): Gene = {
    val grid = mkGridFromInput(input)

    val robots = new util.ArrayList[Robot]

    robotsInputs.foreach { line =>
      val Array(_x, _y, direction) = line split " "
      val x = _x.toInt
      val y = _y.toInt
      val robot = new Robot()
      robot.id = Robot.globalId
      Robot.globalId += 1
      robot.cell = get(grid, x, y)
      robot.direction = charToType(direction.head)
      robots.add(robot)
    }

    applySolution(chromosome, robots, grid)

    val engine = new Engine(robots, grid)
    engine.playToEnd()
    val score = engine.score
    score
  }

  if(Constants.isLocal){
        val rawInput =
            """#........L........#
              |#........R........#
              |#........L........#
              |#........R........#
              |#DUDUDUDU#UDUDUDUD#
              |#........R........#
              |#........L........#
              |#........R........#
              |#........L........#
              |###################
              |4
              |1 0 R
              |17 0 D
              |1 8 U
              |17 8 L
            """.stripMargin

        val sr = new StringReader(rawInput)
        Console.withIn(sr){
            run
        }
    } else {
        run
    }

    private def run = {
        Console.err.println("START_INPUT")

        def getLine() = scala.io.StdIn.readLine()

        val input = 0 until 10 map (_ => {
            val l = getLine()
            Console.err.println(l)
            l
        }) map (_.toArray)

        val robotcount = readInt
        Console.err.println(robotcount)
        val robotsInputs = 0 until robotcount map { _ =>
            val l = getLine()
            Console.err.println(l)
            l
        }

        Console.err.println("END_INPUT")

        //    Console.err.println(input.map(_.mkString).mkString("\n"))

        val (solution, score): (List[ArrowAction], Int) = findBestSolution(input.toArray, robotsInputs)

        val solutionStr = solution
          .map(action => s"${action.x} ${action.y} ${typeToChar(action.dir)}")
          .mkString(" ")

        Console.err.println(s"Expected score: $score")

        println(solutionStr)
    }


}
