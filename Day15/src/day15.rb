require 'test/unit'

class Coordinates
  def initialize(x, y)    
    @x=x
  end
  def x
    @x
  end
end

class Grid
  @size
  def initialize(size)
    @size=size
  end

  def size
    @size
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
    assert_equal(7, grid.size.y)
    
  end
end