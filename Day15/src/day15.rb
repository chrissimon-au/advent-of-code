require 'test/unit'

class Coordinates
  def initialize(x, y)    
  end
  def x
  end
end

class Grid
  @size
  def initialize(size)
  end

  def size
    Coordinates.new(0, 0)
  end
end
   


class MyTest < Test::Unit::TestCase
  # def setup
  # end

  # def teardown
  # end

  def test_grid_layout
    grid = Grid.new(Coordinates.new(6, 7))

    assert_equal(6, grid.size.x)
    
  end
end