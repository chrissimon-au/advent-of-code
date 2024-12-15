require 'test/unit'

class Coordinates
  def initialize(x, y)    
    @x=x
    @y=y
  end
  def x
    @x
  end
  def y
    @y
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
    size=Coordinates.new(6, 7)
    grid = Grid.new(size)

    assert_equal(6, grid.size.x)
    assert_equal(7, grid.size.y)
    
  end
end