import java.util

import Robot.State

import math._
import scala.util._

import scala.collection.JavaConversions._

object Constants {
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



class Engine(val input: String) {
    var score = 0
    // Initialize an empty map
    var grid = Array.tabulate[Cell](MAP_WIDTH, MAP_HEIGHT)((x:Int,y:Int) => {
        val c = new Cell(x,y, Cell.globalId)
        Cell.globalId += 1
        c
    })
    var robots = new util.ArrayList[Robot]
    var wrecks = new util.HashSet[Robot]

    // Link cells
    0 until MAP_WIDTH foreach { x =>
        0 until MAP_HEIGHT foreach { y =>
            val cell = get(x, y)
            cell.nexts(UP) = get(x, y - 1)
            cell.nexts(RIGHT) = get(x + 1, y)
            cell.nexts(DOWN) = get(x, y + 1)
            cell.nexts(LEFT) = get(x - 1, y)
        }
    }

    // Place void cells, robots and arrows
    var index = 0

    0 until MAP_WIDTH foreach { x =>
        0 until MAP_HEIGHT foreach { y =>
            val c = input.charAt(index)
            val cell = get(x, y)
            if (Character.isUpperCase(c)) {
                val robot = new Robot()
                robot.id = Robot.globalId
                Robot.globalId += 1
                robot.cell = cell
                robot.direction = charToType(c)
                robots.add(robot)
            }
            else cell.`type` = charToType(c)
            index += 1
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

    def registerStates(): Unit = {
        for (robot <- robots) {
            robot.registerState
        }
    }

    def apply(x: Int, y: Int, direction: Int): Unit = {
        val cell = get(x, y)
        cell.`type` = direction
        // Check if we need to update a robot direction
        for (robot <- robots) {
            if (robot.cell eq cell) robot.direction = direction
        }
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
}

object Player extends App {
    for(i <- 0 until 10) {
        val line = readLine
    }
    val robotcount = readInt
    for(i <- 0 until robotcount) {
        val Array(_x, _y, direction) = readLine split " "
        val x = _x.toInt
        val y = _y.toInt
    }
    
    // Write an action using println
    // To debug: Console.err.println("Debug messages...")
    
    println("0 0 U 1 1 R 2 2 D 3 3 L")
}
