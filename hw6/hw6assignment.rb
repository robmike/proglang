# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyTetris < Tetris
  # your enhancements here
  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_clockwise; @board.rotate_clockwise})
    @root.bind('c', proc {@board.cheat})
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
end

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  # your enhancements here
  All_My_Pieces = All_Pieces  +
    [[[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]],
      [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
     rotations([[0, 0], [0, 1], [1, 0]]), # small l
     rotations([[0, 0], [-1, 0], [1, 0], [0, -1], [-1, -1]])]

  def self.set_piece(point_array, board)
    MyPiece.new(point_array, board)
  end

  def self.next_piece (board)
    self.set_piece(All_My_Pieces.sample, board)
  end

end

class MyBoard < Board
  # your enhancements here
  Cheat_Cost = 10

  def cheat
    if @score >= Cheat_Cost and @current_block != Cheat_Piece
      puts "cheating"
      @score -= Cheat_Cost
      @cheat_next_piece = true
    end
    puts @cheat_next_piece
  end

  def next_piece
    puts "my next_piece"
    if @cheat_next_piece
      @current_block = MyPiece.new([[[0,0]]], self)
      @cheat_next_piece = false
    else
      @current_block = MyPiece.next_piece(self) 
    end
    @current_pos = nil
  end
end
