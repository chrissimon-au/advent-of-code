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
  def hash
    to_s.hash
  end
  def test_move(coords)
    Coordinates.new(coords.x+@x, coords.y+@y)
  end
  def within(boundary)
    @x>=0 && @y>=0 && @x<boundary.x && @y<boundary.y
  end
  def to_s
    "(#{@x},#{@y})"
  end
  def gps
    @y*100 + @x
  end
end

class Grid
  
  def initialize(size, robot=nil)
    @size=size
    @robot=robot
    @walls = Set[]
    @boxes = Set[]
  end
  
  def self.parse(map_input)
    parts = map_input.split("\n\n")
    map = parts[0]
    instructions = parts[1]
    
    rows = map.split("\n")
    size = Coordinates.new(rows[0].length, rows.length)
    grid = Grid.new(size)

    rows.each.with_index do |row, rowIdx|
      row.split("").each.with_index do |cell, cellIdx|
        case cell
        when "#"
          grid.add_wall(Coordinates.new(cellIdx, rowIdx))
        when "@"
          grid.set_robot(Coordinates.new(cellIdx, rowIdx))
        when "O"
          grid.add_box(Coordinates.new(cellIdx, rowIdx))
        end
      end
    end
    grid
  end

  def size
    @size
  end
  def robot
    @robot
  end

  def set_robot(robot)
    @robot = robot
  end

  def is_pos_free(pos)
    (!@walls.include?(pos)) &&
    (!@boxes.include?(pos)) &&
    pos.within(size)
  end

  def gather_boxes_until_space(movement)
    boxes_until_space=Set[]
    test_pos = robot.test_move(movement)
    while (!is_pos_free(test_pos) && @boxes.include?(test_pos))
      boxes_until_space.add(test_pos)
      test_pos = test_pos.test_move(movement)
    end
    if (!is_pos_free(test_pos)) then
      return Set[]
    end
    return boxes_until_space    
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
    if is_pos_free(test_new_pos) then
      @robot = test_new_pos
    else
      boxes_in_way = gather_boxes_until_space(movement)
      if !boxes_in_way.empty? then
        @robot = test_new_pos
        boxes_in_way.each do |box|
          @boxes.delete(box)
        end
        boxes_in_way.each do |box|
          @boxes.add(box.test_move(movement))
        end
      end
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

  def add_box(box)
    @boxes.add(box)
  end

  def is_box_at(coords)
    @boxes.include?(coords)
  end

  def is_wall_at(coords)
    @walls.include?(coords)
  end

  def gps
    total_gps=0
    @boxes.each do |box|
      total_gps+=box.gps
    end
    total_gps
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

  def test_multiple_walls_blocks_robot
    size=Coordinates.new(6, 7)
    robot=Coordinates.new(2, 4)
    grid = Grid.new(size, robot)
    grid.add_wall(Coordinates.new(3,4))
    grid.add_wall(Coordinates.new(4,5))
    
    grid.move_robot(">v>>^")
    assert_equal(3, grid.robot.x)
    assert_equal(5, grid.robot.y)
  end

  def test_edges_block_robot
    size=Coordinates.new(6, 7)
    robot=Coordinates.new(5, 4)
    grid = Grid.new(size, robot)
    
    grid.move_robot(">>>vvv<<<<<<<<^^^^^^^^^^>>>>>>>>>")
    assert_equal(5, grid.robot.x)
    assert_equal(0, grid.robot.y)
  end

  def test_boxes_are_moved
    size=Coordinates.new(6, 7)
    robot=Coordinates.new(2, 4)
    grid = Grid.new(size, robot)

    grid.add_box(Coordinates.new(3,4))
    grid.add_box(Coordinates.new(4,4))

    assert(grid.is_box_at(Coordinates.new(3,4)))
    assert(grid.is_box_at(Coordinates.new(4,4)))    
    assert_false(grid.is_box_at(Coordinates.new(5,4)))

    grid.move_robot(">")

    assert_equal(3, grid.robot.x)
    assert_equal(4, grid.robot.y)
    
    assert_false(grid.is_box_at(Coordinates.new(3,4)))
    assert(grid.is_box_at(Coordinates.new(4,4)))    
    assert(grid.is_box_at(Coordinates.new(5,4)))
  end

  def test_box_movement_blocked_by_wall
    size=Coordinates.new(6, 7)
    robot=Coordinates.new(2, 4)
    grid = Grid.new(size, robot)

    grid.add_box(Coordinates.new(3,4))
    grid.add_wall(Coordinates.new(4,4))

    assert(grid.is_box_at(Coordinates.new(3,4)))

    grid.move_robot(">")

    assert_equal(2, grid.robot.x)
    assert_equal(4, grid.robot.y)
    
    assert(grid.is_box_at(Coordinates.new(3,4)))
  end

  def test_box_movement_blocked_by_wall
    size=Coordinates.new(6, 7)
    robot=Coordinates.new(4, 4)
    grid = Grid.new(size, robot)

    grid.add_box(Coordinates.new(5,4))

    assert(grid.is_box_at(Coordinates.new(5,4)))

    grid.move_robot(">")

    assert_equal(4, grid.robot.x)
    assert_equal(4, grid.robot.y)
    
    assert(grid.is_box_at(Coordinates.new(5,4)))
  end

  def test_get_gps_of_grid
    size=Coordinates.new(6, 7)
    robot=Coordinates.new(4, 4)
    grid = Grid.new(size, robot)
    grid.add_box(Coordinates.new(4,1))

    assert_equal(104, grid.gps)
  end

  def test_parse_map
    map_input = <<~EOS
                  ########
                  #..O.O.#
                  ##@.O..#
                  #...O..#
                  #.#.O..#
                  #...O..#
                  #......#
                  ########
                EOS
    grid = Grid.parse(map_input)

    assert_equal(2, grid.robot.x)
    assert_equal(2, grid.robot.y)
    assert(grid.is_wall_at(Coordinates.new(0, 0)))
    assert(grid.is_wall_at(Coordinates.new(1, 2)))
    assert(grid.is_wall_at(Coordinates.new(2, 4)))
    assert(grid.is_box_at(Coordinates.new(3,1)))
    assert(grid.is_box_at(Coordinates.new(5,1)))
    assert(grid.is_box_at(Coordinates.new(4,2)))
    assert(grid.is_box_at(Coordinates.new(4,3)))
    assert(grid.is_box_at(Coordinates.new(4,4)))
    assert(grid.is_box_at(Coordinates.new(4,5)))
  end
end
