package edu.gsu.cs.butterfly.exec;

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
 * Prted to java by Alex Artyomenko on 3/31/2014.
 */

// This file contains the class Ukkonen. Which provides a method editCost()
// that calculates the edit distance between two strings using Ukkonen's
// algorithm in O(nd) time and O(d) space, but no alignment information is
// determined.
final class Ukkonen {
    private static int BIG_NEGATIVE = -10;
    private String A, B;            // Two strings to be aligned
    private int[][] data;        // 2 dimensional array of 'cost' rows,
    // and 3 columns.

    private int offset = 0;            // Offset into data[] because -ve indices
    // needed.
    private int innerLoop = 0, outerLoop = 0;

    private static int max3(int x, int y, int z) {
        return (x >= y ? (x >= z ? x : z) : (y >= z ? y : z));
    }

    private int Ukk(int diag, int cost) {
        int v1, v2, v3, res;

        if (diag == 0 && cost == -1) return -1; // Starting condition
        if (Math.abs(diag) > cost) return BIG_NEGATIVE; // Boundary condition

        if (data[diag + offset][cost % 3] >= 0)    // Check if already calculated
            return data[diag + offset][cost % 3];

        v1 = Ukk(diag + 1, cost - 1);
        v2 = Ukk(diag, cost - 1) + 1;
        v3 = Ukk(diag - 1, cost - 1) + 1;
        res = max3(v1, v2, v3);

        while (res < A.length() && res - diag < B.length() && A.charAt(res) == B.charAt(res - diag)) { // Extend diagonal while matching
            res++;
            innerLoop++;
        }
        outerLoop++;

        data[diag + offset][cost % 3] = res;

        return res;
    }

    /**
     * Compute edit distance between two
     * strings usnig Ukkonen's method
     *
     * @param strA Argument string A
     * @param strB Argument string B
     * @return edit distance (int)
     */
    public int editCost(String strA, String strB) {
        int cost, res, maxCost;
        int lenA, lenB, finalDiag;

        innerLoop = outerLoop = 0;

        A = strA;
        B = strB;
        lenA = A.length();
        lenB = B.length();
        finalDiag = lenA - lenB;

        maxCost = (lenA > lenB) ? lenA : lenB;
        offset = (maxCost - (lenA - lenB)) / 2; // Offset to allow for -ve diagonals
        // Now allocate storage. Largest possible edit distance by 3 columns needed
        data = new int[maxCost + 1][3];

        for (int i = 0; i <= maxCost; i++) { // Initialize the structure
            data[i][0] = BIG_NEGATIVE;
            data[i][1] = BIG_NEGATIVE;
            data[i][2] = BIG_NEGATIVE;
        }

        cost = 0;
        do {
            // Remove data for next iteration
            for (int i = cost - 3, j = finalDiag; i >= Math.abs(j); i--, j++)
                data[j + offset][i % 3] = BIG_NEGATIVE;
            for (int i = cost - 3, j = finalDiag; i >= Math.abs(j); i--, j--)
                data[j + offset][i % 3] = BIG_NEGATIVE;

            res = Ukk(finalDiag, cost);
            cost++;

        } while (res != lenA);

        return cost - 1;
    }
}

