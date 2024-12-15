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
  
  def self.parse(map_input, double=false)
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

    if instructions then
      grid.move_robot(instructions)
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
    if !movement then
      return
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