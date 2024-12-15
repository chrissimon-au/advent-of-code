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
  def move(coords)
    @x+=coords.x
  end
end

class Grid
  def initialize(size, robot=nil)
    @size=size
    @robot=robot
  end

  def size
    @size
  end
  def robot
    @robot
  end

  def move_robot(instructions)
    case instructions
    when ">"
      robot.move(Coordinates.new(1,0))
    end    
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

  def test_robot_position
    size=Coordinates.new(6, 7)
    robot=Coordinates.new(2, 3)
    grid = Grid.new(size, robot)

    assert_equal(2, grid.robot.x)
    assert_equal(3, grid.robot.y)
  end

  def test_move_robot
    size=Coordinates.new(6, 7)
    robot=Coordinates.new(2, 4)
    grid = Grid.new(size, robot)

    assert_equal(2, grid.robot.x)
    assert_equal(4, grid.robot.y)
    grid.move_robot(">")
    assert_equal(3, grid.robot.x)
    assert_equal(4, grid.robot.y)
    grid.move_robot("v")
    assert_equal(3, grid.robot.x)
    assert_equal(5, grid.robot.y)
    grid.move_robot("<")
    assert_equal(2, grid.robot.x)
    assert_equal(5, grid.robot.y)
    grid.move_robot("^")
    assert_equal(2, grid.robot.x)
    assert_equal(4, grid.robot.y)
  end
end