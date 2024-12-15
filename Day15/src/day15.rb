require 'test/unit'
require 'set'

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
  def eql?(other)
    @x == other.x && @y == other.y
  end
  alias :== eql?
  def hash()
    "#{@x},#{@y}".hash
  end
  def test_move(coords)
    Coordinates.new(coords.x+@x, coords.y+@y)
  end
end

class Grid
  
  def initialize(size, robot=nil)
    @size=size
    @robot=robot
    @walls = Set[]
  end

  def size
    @size
  end
  def robot
    @robot
  end

  def move_robot_single(movement_instruction)
    movement =
      case movement_instruction
      when ">"
        Coordinates.new(1,0)
      when "v"
        Coordinates.new(0,1)
      when "<"
        Coordinates.new(-1,0)
      when "^"
        Coordinates.new(0,-1)
      end
    test_new_pos = robot.test_move(movement)
    if (!@walls.include?(test_new_pos)) then
      @robot = test_new_pos
    end
  end

  def move_robot(instructions)
    instructions.split("").each do |movement|
      move_robot_single(movement)
    end    
  end

  def add_wall(wall)
    @walls.add(wall)
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

  def test_move_robot_multiple_instructions
    size=Coordinates.new(6, 7)
    robot=Coordinates.new(2, 4)
    grid = Grid.new(size, robot)

    assert_equal(2, grid.robot.x)
    assert_equal(4, grid.robot.y)
    grid.move_robot(">v>>v<^")
    assert_equal(4, grid.robot.x)
    assert_equal(5, grid.robot.y)    
  end

  def test_wall_blocks_robot
    size=Coordinates.new(6, 7)
    robot=Coordinates.new(2, 4)
    grid = Grid.new(size, robot)
    grid.add_wall(Coordinates.new(3,4))
    
    grid.move_robot(">")
    assert_equal(2, grid.robot.x)
    assert_equal(4, grid.robot.y)
  end
end