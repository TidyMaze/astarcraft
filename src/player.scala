import java.util

import Player.input
import Robot.State

import math._
import scala.util._
import scala.collection.JavaConversions._
import scala.collection.immutable
import scala.language.postfixOps


object Constants {
    val TimesNone = 4
    val MaxTime = 950

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

case class ArrowAction(x: Int, y: Int, dir: Int)

object Player extends App {
    def get(grid: Array[Array[Cell]], x: Int, y: Int): Cell = {
        var x2 = x
        var y2 = y
        if (x2 < 0) x2 += MAP_WIDTH
        else if (x2 >= MAP_WIDTH) x2 -= MAP_WIDTH
        if (y2 < 0) y2 += MAP_HEIGHT
        else if (y2 >= MAP_HEIGHT) y2 -= MAP_HEIGHT
        grid(x2)(y2)
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

    def getRandomSolution(grid: Array[Array[Cell]]): List[ArrowAction] = {
        val nones = List.fill(TimesNone)(NONE)
        val basicDirs = UP :: DOWN :: LEFT :: RIGHT :: Nil
        grid
          .flatten
          .toList
          .collect({
              case c: Cell if c.`type` == NONE =>
                  ArrowAction(c.x, c.y, randomFrom(nones ++ basicDirs.flatMap(e => if (c.nexts(e).`type` != VOID) Some(e) else None)))
          })
          .filter(_.dir != NONE)
    }

    def applySolution(solution: List[ArrowAction], robots: util.ArrayList[Robot], grid: Array[Array[Cell]]): Unit =
        solution.foreach(action => apply(grid, robots, action.x, action.y, action.dir))


    def parseGenSolutionAndScore(grid: Array[Array[Cell]], input: Array[Array[Char]], robotsInputs: Seq[String]): (List[ArrowAction], Int) = {
        0 until MAP_HEIGHT foreach { y =>
            0 until MAP_WIDTH foreach { x =>
                val c = input(y)(x)
                val cell = get(grid, x, y)
                cell.`type` = charToType(c)
            }
        }


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

        val solution: List[ArrowAction] = getRandomSolution(grid)

        applySolution(solution, robots, grid)

        val engine = new Engine(robots, grid)
        engine.playToEnd()
        val score = engine.score

        (solution, score)
    }

    def findBestSolution(input: Array[Array[Char]], robotsInputs: Seq[String]) = {
      val start = System.currentTimeMillis()

      // Initialize an empty map
      val grid = Array.tabulate[Cell](MAP_WIDTH, MAP_HEIGHT)((x:Int,y:Int) => {
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

        var bestSolution: List[ArrowAction] = null
        var bestScore = 0
        while ((System.currentTimeMillis() - start) < MaxTime){
            val (solution, score) = parseGenSolutionAndScore(grid, input, robotsInputs)
            if(score >= bestScore){
                bestScore = score
                bestSolution = solution
            }
        }
        (bestSolution, bestScore)
    }


    val input = 0 until 10 map (_ => readLine) map (_.toArray)

    val robotcount = readInt
    val robotsInputs = 0 until robotcount map { _ =>
        readLine
    }

    Console.err.println(input.map(_.mkString).mkString)

    val (solution, score): (List[ArrowAction], Int) = findBestSolution(input.toArray, robotsInputs)

    val solutionStr = solution
      .map(action => s"${action.x} ${action.y} ${typeToChar(action.dir)}")
      .mkString(" ")

    Console.err.println(s"Expected score: $score")

    println(solutionStr)


}
