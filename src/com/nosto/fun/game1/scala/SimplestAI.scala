/**
 * Brute force algorithm based on weight for 5-in-a-row game.
 * According to the position of the latest opponent's piece,
 * calculate the weight of the place where the AI around it can play, and return the position with the highest weight。
 *
 * It only defence the opponet, doesn't consider its own move, but also very interactive。
 * @author Wen
 */
package com.nosto.fun.game1.scala
import com.nosto.fun.game1.{ArenaPosition, Piece}
import scala.util.Random

class SimplestAI(name: String) extends ScalaPlayer with Cloneable{
  private var myPiece: Piece = null
  override def setSide(piece: Piece): Unit = {
    myPiece = piece
  }
  override def getSide(): Piece = myPiece

  override def getName(): String = name

  val SIZE = 18 // The length of row and col
  /**
   * Weight Value
   * Based on the different situations, give the blank slots different weights to determine the next step.
   * X -- Opponent pieces
   * O -- AI pieces
   *
   * ALIVE_1: **X**
   * ALIVE_2: **XX**
   * ALIVE_3: **XXX**
   * ALIVE_4: **XXXX**
   *
   * HALF_ALIVE_1: **XO**
   * HALF_ALIVE_2: **XXO**
   * HALF_ALIVE_3: **XXXO**
   * HALF_ALIVE_4: **XXXXO**
   */
  val ALIVE_1: Int = 10
  val ALIVE_2: Int = 100
  val ALIVE_3: Int = 1000
  val ALIVE_4: Int = 10000
  val HALF_ALIVE_1: Int = 2
  val HALF_ALIVE_2: Int = 8
  val HALF_ALIVE_3: Int = 50
  val HALF_ALIVE_4: Int = 10000


//  If a slot is occupied by opponent, the value is 1; if empty, value is 0; else, 2.
  private val chessArray = Array.ofDim[Int](SIZE, SIZE)

  var weightArray: Array[Array[Int]] = Array.ofDim[Int](SIZE, SIZE)


  override def move(board: Array[Array[Piece]], last: ArenaPosition): ArenaPosition = {
//    emptySlots return total empty slots on the current board.
    val emptySlots = board.map(_.zipWithIndex.collect {
      case (null, index) => index
    }).zipWithIndex.flatMap {
      case (rows, col) => rows.map((_, col))
    }

//    Last position always return the opponent position
    val currentRow = last.getRow
    val currentCol = last.getColumn
    chessArray(currentRow)(currentCol) = 1
    /* Before computing the weights, clean the weight array. */
    weightArray = Array.fill(SIZE, SIZE)(0)

    /**
     * Eight directions in total, but compute weights need to flat the direction, like Horizontal direction has two directions.
     */
    weightArray = countHorizon(chessArray, weightArray, currentRow, currentCol)
    weightArray = countVertical(chessArray, weightArray, currentRow, currentCol)
    weightArray = countLeftDiagonal(chessArray, weightArray, currentRow, currentCol)
    weightArray = countRightDiagonal(chessArray, weightArray, currentRow, currentCol)

//    Get the row and column of the biggest weight.
    val r: Int = findBiggestWeight(weightArray)._1
    val c: Int = findBiggestWeight(weightArray)._2
//    In my findBiggestWeight, if there is no biggest weight, use random.
    if (r == 0 && c == 0) {
      emptySlots(Random.nextInt(emptySlots.size)) match {
        case (row, col) =>
          new ArenaPosition(col, row)
      }
    } else {
      chessArray(r)(c) = 2
      new ArenaPosition(r, c)
    }
  }

  /**
   * This function is compute the weights of horizontal direction based the current piece location.
   * Use the weight values defined before, like ALIVE_2 or HALF_ALIVE_2.
   *
   * @param chessArray The whole board flags, opponent piece: 1, AI piece 2, empty piece 0/
   * @param weightArray The weightArray arround the current opponent locations.
   * @param row The row value of the current opponent locations.
   * @param col The column value of the current opponent locations.
   * @return ArenaPosition instance that describes on which board cell you want to place your piece.
   */
  def countHorizon(chessArr: Array[Array[Int]], weightArray: Array[Array[Int]], row: Int, col: Int): Array[Array[Int]] = {
    var count = 1
    //    Count in the right direction.
    var rightSlot = if (col+1 < SIZE) col + 1 else SIZE - 1
    while (rightSlot < SIZE && chessArr(row)(rightSlot) == 1 ){
      rightSlot += 1
      count += 1
    }
//    In case the index out the boundary
    rightSlot =  if(rightSlot == SIZE) SIZE - 1 else  rightSlot
//  Count in the left direction.
    var leftSlot =  if (col-1 > 0) col - 1 else 0
    while (leftSlot >= 0 && chessArr(row)(leftSlot) == 1){
      leftSlot -= 1
      count += 1
    }
    leftSlot = if (leftSlot < 0) 0 else leftSlot
// Determine this direction is alive or half alive.
    if (chessArr(row)(leftSlot) != 0 && chessArr(row)(rightSlot) == 0  ) weightArray(row)(rightSlot) = countMatchFunction(count, false)
    else if (chessArr(row)(leftSlot)== 0 && chessArr(row)(rightSlot) != 0 ) weightArray(row)(leftSlot) = countMatchFunction(count, false)
    else if (chessArr(row)(leftSlot)== 0 && chessArr(row)(rightSlot) == 0){
      weightArray(row)(rightSlot) = countMatchFunction(count, true)
      weightArray(row)(leftSlot) = countMatchFunction(count, true)
    }
    weightArray
  }
  /**
   * Compute the weights of vertical direction based the current piece location.
   */
  def countVertical(chessArr: Array[Array[Int]], weightArray: Array[Array[Int]], row: Int, col: Int): Array[Array[Int]] = {
    var count = 1
//    Count the down direction
    var downSlot: Int = if (row + 1 < SIZE) row + 1 else SIZE - 1
    while (downSlot < SIZE && chessArr(downSlot)(col) == 1 ){
      downSlot += 1
      count += 1
    }
    downSlot = if(downSlot == SIZE) SIZE - 1 else downSlot
//    Count the up direction
    var upSlot: Int =  if (row - 1 > 0) row - 1 else 0
    while (upSlot >= 0 && chessArr(upSlot)(col) == 1 ){
      upSlot -= 1
      count += 1
    }
    upSlot = if (upSlot < 0) 0 else upSlot

    if (chessArr(downSlot)(col) != 0 && chessArr(upSlot)(col) == 0) weightArray(upSlot)(col) = countMatchFunction(count, aliveFlag = false)
    else if (chessArr(downSlot)(col)== 0 && chessArr(upSlot)(col) != 0 ) weightArray(downSlot)(col) = countMatchFunction(count, aliveFlag = false)
    else if (chessArr(downSlot)(col)== 0 && chessArr(upSlot)(col) == 0){
      weightArray(downSlot)(col) = countMatchFunction(count, aliveFlag = true)
      weightArray(upSlot)(col)= countMatchFunction(count, aliveFlag = true)
    }
    weightArray
  }

  /**
   * Compute the weights of the two diagonal directions based the current piece location.
   */
  def countLeftDiagonal(chessArr: Array[Array[Int]], weightArray: Array[Array[Int]], row: Int, col: Int): Array[Array[Int]] = {
    var leftDiagCount= 1
    var rightSlot: Int = if (col+1 < SIZE) col + 1 else SIZE - 1
    var leftSlot: Int =  if (col-1 > 0) col - 1 else 0
    var downSlot: Int = if (row + 1 < SIZE) row + 1 else SIZE - 1
    var upSlot: Int =  if (row - 1 > 0) row - 1 else 0
//  Count in the left-down direction
    while (leftSlot >= 0 && downSlot < SIZE && chessArr(downSlot)(leftSlot) == 1){
      leftDiagCount += 1
      leftSlot -= 1
      downSlot += 1
    }
//    Boundary check
    leftSlot = if (leftSlot < 0) 0 else leftSlot
    downSlot = if (downSlot >= SIZE) SIZE - 1 else downSlot
//     Count in the right-up direction
    while (rightSlot < SIZE && upSlot >= 0 && chessArr(upSlot)(rightSlot) == 1 ){
      leftDiagCount += 1
      rightSlot += 1
      upSlot -= 1
    }
//      Boundary check
    rightSlot = if(rightSlot >= SIZE) SIZE - 1 else rightSlot
    upSlot = if (upSlot < 0) 0 else upSlot
    if (chessArr(downSlot)(leftSlot) != 0 && chessArr(upSlot)(rightSlot) == 0  ){
      weightArray(upSlot)(rightSlot) = countMatchFunction(leftDiagCount, aliveFlag = false)
    }else if (chessArr(downSlot)(leftSlot)== 0 && chessArr(upSlot)(rightSlot) != 0 ) {
      weightArray(downSlot)(leftSlot) = countMatchFunction(leftDiagCount, aliveFlag = false)
    } else if (chessArr(downSlot)(leftSlot)== 0 && chessArr(upSlot)(rightSlot) == 0){
      weightArray(downSlot)(leftSlot) = countMatchFunction(leftDiagCount, aliveFlag = true)
      weightArray(upSlot)(rightSlot)= countMatchFunction(leftDiagCount, aliveFlag = true)
    }
    weightArray
  }

  def countRightDiagonal(chessArr: Array[Array[Int]], weightArray: Array[Array[Int]], row: Int, col: Int): Array[Array[Int]] ={
    var rightDiagCount = 1
    var rightSlot: Int = if (col+1 < SIZE) col + 1 else SIZE - 1
    var leftSlot: Int =  if (col-1 > 0) col - 1 else 0
    var downSlot: Int = if (row + 1 < SIZE) row + 1 else SIZE - 1
    var upSlot: Int =  if (row - 1 > 0) row - 1 else 0
//    Count in the left-up direction
    while (leftSlot >= 0 && upSlot >= 0 &&chessArr(upSlot)(leftSlot) == 1  ){
      rightDiagCount += 1
      leftSlot -= 1
      upSlot -= 1
    }
    leftSlot = if (leftSlot <= 0) 0 else leftSlot
    upSlot = if (upSlot <= 0) 0 else upSlot
//    Count in the right-down direction
    while (downSlot < SIZE && rightSlot < SIZE && chessArr(downSlot)(rightSlot) == 1 ){
      rightDiagCount += 1
      downSlot += 1
      rightSlot += 1
    }
    downSlot = if (downSlot >= SIZE) SIZE - 1 else downSlot
    rightSlot = if(rightSlot >= SIZE) SIZE - 1  else rightSlot

    if (chessArr(upSlot)(leftSlot) != 0 && chessArr(downSlot)(rightSlot) == 0  ){
      weightArray(downSlot)(rightSlot) = countMatchFunction(rightDiagCount, aliveFlag = false)

    }else if (chessArr(upSlot)(leftSlot)== 0 && chessArr(downSlot)(rightSlot) != 0 ) {
      weightArray(upSlot)(leftSlot) = countMatchFunction(rightDiagCount, aliveFlag = false)
    } else if (chessArr(upSlot)(leftSlot)== 0 && chessArr(downSlot)(rightSlot) == 0){

      weightArray(upSlot)(leftSlot) = countMatchFunction(rightDiagCount, aliveFlag = true)
      weightArray(downSlot)(rightSlot)= countMatchFunction(rightDiagCount, aliveFlag = true)
    }
    weightArray
  }

  /**
   * This function is a match function which returns the weights based on the number of opponent pieces.
   * @param count the number of opponent pieces
   * @param aliveFlag estimate the situation is alive or half alive.
   * @return weights.
   */
  def countMatchFunction(count: Int, aliveFlag: Boolean): Int = {
    val weightValue: Int = if (aliveFlag ){
//      Can't be 5 because game is already over.
      count match {
        case 1 => ALIVE_1
        case 2 => ALIVE_2
        case 3 => ALIVE_3
        case 4 => ALIVE_4
        case _ => 0
      }
    } else {
      count match {
        case 1 => HALF_ALIVE_1
        case 2 => HALF_ALIVE_2
        case 3 => HALF_ALIVE_3
        case 4 => HALF_ALIVE_4
        case _ => 0
      }

    }
    weightValue
  }

  /**
   * Find the biggest weight and return the row and column.
   * @param weightArraay
   * @return Tuple2(row, column)
   */
  def findBiggestWeight(weightArr: Array[Array[Int]]): (Int, Int) = {
    var maxWeight = weightArr(0)(0)
    var row: Int = 0
    var col: Int = 0
    for (i <- 0 until  SIZE ){
      for (j <- 0 until  SIZE ){
        if (weightArr(i)(j) > maxWeight) {
          maxWeight = weightArr(i)(j)
          row = i
          col = j
        }
      }
    }
    Tuple2(row, col)
  }

}
