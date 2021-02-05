package NamLA.leetCode

object Sudoku {

    def solveSudoku(board: Array[Array[Char]]): Unit = {
        solve(board)
    }

    def solve(board: Array[Array[Char]], x: Int = 0, y: Int = 0) : Boolean = {
        if (x == board.length)
            return true
        if (board(x)(y) != '.') {
            if (x == board.length - 1 && y == board.head.length - 1)
                return true
            return solve(board, x + (y + 1)/board.head.length, (y + 1) % board.head.length)
        }
        // fill 1 to 9
        for (c <- '1' to '9') {
            if (isCanFill(board, x, y, c)) {
                board(x)(y) = c
                if (solve(board, x + (y + 1)/board.head.length, (y + 1) % board.head.length))
                    return true
                board(x)(y) = '.'
            }
        }
        false
    }

    def isCanFill(board: Array[Array[Char]], x: Int = 0, y: Int = 0, c: Char = '1'): Boolean = {
        val blockRow = (x / 3) * 3
        val blockCol = (y / 3) * 3
        for (i <- 0 to 8) {
            if (board(i)(y) == c || board(x)(i) == c || board(blockRow + i / 3)(blockCol + i % 3) == c)
                return false
        }
        true
    }

    def main(args: Array[String]): Unit = {

    }
}
