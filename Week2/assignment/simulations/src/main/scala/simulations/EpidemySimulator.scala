package simulations

import math.random

class EpidemySimulator extends Simulator {
  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val incubationTime = 6
    val dieTime = 14
    val immuneTime = 16
    val healTime = 18

    val prevalenceRate = 0.01
    val transRate = 0.4
    val dieRate = 0.25
  }

  import SimConfig._

  val persons: List[Person] =  Range(1, SimConfig.population).map( i => {
    val person = new Person(i)
    if(i <= (population * prevalenceRate)) person.getInfected

    person
  }).toList

  persons.foreach(_.move)

  def sickPeopleAt (row:Int, col:Int, person:Person) = {
    persons.exists(p => p.row == row && p.col == col && p.id != person.id && p.visiblyInfectuous)
  }

  def infectedPeopleAt (row:Int, col:Int, person:Person) = {
    persons.exists(p => p.row == row && p.col == col && p.id != person.id && p.infected)
  }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    def visiblyInfectuous = (sick || dead)

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def Up = {
      row match {
        case 0 => (7, col)
        case _ => (row -1, col)
      }
    }

    def Down = {
      row match {
        case 7 => (0, col)
        case _ => (row +1, col)
      }
    }

    def Left = {
      col match {
        case 0 => (row, 7)
        case _ => (row, col - 1)
      }
    }

    def Right = {
      col match {
        case 7 => (row, 0)
        case _ => (row, col + 1)
      }
    }

    def Days = randomBelow(5) + 1

    def getNeighbor = {
      val available = List(Up, Down, Left, Right).filter(c => !sickPeopleAt(c._1, c._2, this))

      if(available.isEmpty) None
      else Some(available(randomBelow(available.length)))
    }


    def move:Unit = {
      afterDelay(Days) {
        if(!dead){
          val neighbor = getNeighbor

          neighbor.map(n => {
            row = n._1
            col = n._2

            if(infectedPeopleAt(n._1, n._2, this)){
              if(!immune && !infected && random < transRate)
                getInfected
            }
          })

          move
        }
      }
    }

    def getInfected = {
      if(!dead){
        infected = true

        afterDelay (incubationTime) (getSick)
        afterDelay (dieTime) (die)
        afterDelay (immuneTime) (getImmune)
        afterDelay (healTime) (getHealthy)
      }

    }

    def getSick = {
      if(!dead)
        sick = true
    }

    def die = {
      if(!dead)
        if(random < dieRate) dead = true
    }

    def getImmune = {
      if(!dead) {
        immune = true; sick = false
      }
    }

    def getHealthy = {
      if(!dead){
        infected = false; immune = false
      }
    }
  }
}
