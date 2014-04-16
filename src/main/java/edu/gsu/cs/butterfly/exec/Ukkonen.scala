package edu.gsu.cs.butterfly.exec

import java.lang.String

/*
 * Copyright (c) David Powell <david@drp.id.au>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 * Prted to scala by Alex Artyomenko on 3/31/2014.
 */
final object Ukkonen {
  private def max3(x: Int, y: Int, z: Int): Int = {
    return (if (x >= y) (if (x >= z) x else z) else (if (y >= z) y else z))
  }

  private val BIG_NEGATIVE: Int = -10

  def editCost(arg1: String, arg2: String): Int = {
    new Ukkonen().editCost(arg1, arg2) - Math.abs(arg1.length - arg2.length)
  }

  final class Ukkonen {
    private def Ukk(diag: Int, cost: Int): Int = {
      var v1: Int = 0
      var v2: Int = 0
      var v3: Int = 0
      var res: Int = 0
      if (diag == 0 && cost == -1) return -1
      if (Math.abs(diag) > cost) return BIG_NEGATIVE
      if (data(diag + offset)(cost % 3) >= 0) return data(diag + offset)(cost % 3)
      v1 = Ukk(diag + 1, cost - 1)
      v2 = Ukk(diag, cost - 1) + 1
      v3 = Ukk(diag - 1, cost - 1) + 1
      res = max3(v1, v2, v3)
      while (res < A.length && res - diag < B.length && A.charAt(res) == B.charAt(res - diag)) {
        res += 1
        innerLoop += 1
      }
      outerLoop += 1
      data(diag + offset)(cost % 3) = res
      return res
    }

    /**
     * Compute edit distance between two
     * strings usnig Ukkonen's method
     *
     * @param strA Argument string A
     * @param strB Argument string B
     * @return edit distance (int)
     */
    def editCost(strA: String, strB: String): Int = {
      var cost: Int = 0
      var res: Int = 0
      var maxCost: Int = 0
      var lenA: Int = 0
      var lenB: Int = 0
      var finalDiag: Int = 0
      innerLoop = ({
        outerLoop = 0;
        outerLoop
      })
      A = strA
      B = strB
      lenA = A.length
      lenB = B.length
      finalDiag = lenA - lenB
      maxCost = if ((lenA > lenB)) lenA else lenB
      offset = (maxCost - (lenA - lenB)) / 2
      data = Array.ofDim(maxCost + 1, 3)
      var i: Int = 0
      while (i <= maxCost) {
        {
          data(i)(0) = BIG_NEGATIVE
          data(i)(1) = BIG_NEGATIVE
          data(i)(2) = BIG_NEGATIVE
        }
        ({
          i += 1;
          i - 1
        })
      }

      cost = 0
      do {
        {
          var i: Int = cost - 3
          var j: Int = finalDiag
          while (i >= Math.abs(j)) {
            data(j + offset)(i % 3) = BIG_NEGATIVE
            ({
              i -= 1;
              i + 1
            })
            ({
              j += 1;
              j - 1
            })
          }
        }
        {
          var i: Int = cost - 3
          var j: Int = finalDiag
          while (i >= Math.abs(j)) {
            data(j + offset)(i % 3) = BIG_NEGATIVE
            ({
              i -= 1;
              i + 1
            })
            ({
              j -= 1;
              j + 1
            })
          }
        }
        res = Ukk(finalDiag, cost)
        cost += 1
      } while (res != lenA)
      return cost - 1
    }

    private var A: String = null
    private var B: String = null
    private var data: Array[Array[Int]] = null
    private var offset: Int = 0
    private var innerLoop: Int = 0
    private var outerLoop: Int = 0
  }

}
