require 'set'

class Coordinates
  def initialize(x, y, item=nil)
    @x=x
    @y=y
    @item=item
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
    Coordinates.new(coords.x+@x, coords.y+@y, @item)
  end
  def within(boundary)
    @x>=0 && @y>=0 && @x<boundary.x && @y<boundary.y
  end
  def item
    @item
  end
  def to_s
    "(#{@x},#{@y})"
  end
  def gps
    @y*100 + @x
  end
end

class Item

  def initialize(coords, width=1)
    @coords = []
    @width=width
    for x in 0...width do
      @coords.push(Coordinates.new((coords.x*width)+x, coords.y, self))
    end
  end

  def add_to_grid(grid)
    @coords.each do |coords|
      grid.add_item(coords)
    end
  end

  def eql?(other)
    to_s() == other.to_s()
  end
  alias :== eql?
  def hash
    to_s.hash
  end

  def to_s
    "#{self.class}: #{@coords.join("; ")}"
  end

  def free_to_move(_, _)
    false
  end
end

class Box < Item

  def remove_from_grid(grid)
    @coords.each do |coords|
      grid.remove_item(coords)
    end
  end

  def new_position(movement)
    new_coords = []
    @coords.each do |coords|
      new_coords.push(coords.test_move(movement))
    end
    new_coords
  end

  def test_move(movement)    
    new_coords = new_position(movement)
    if (movement.x > 0) then
      return [new_coords[@width-1]]
    elsif (movement.x < 0) then
      return [new_coords[0]]
    end
    return new_coords
  end

  def free_to_move(grid, movement)
    new_coords = test_move(movement)
    new_coords.all? do | coords |
      item = grid.item_at(coords)
      item.nil? || (item.is_a? Box)  
    end
  end

  def move(movement)
    @coords = new_position(movement)
  end

  def gps
    @coords[0].gps
  end

  def visualise(pos)
    if @coords.length == 0 then
      print "O"
    elsif @coords[0] == pos then
      print "["
    else
      print "]"
    end
  end

end

class Wall < Item
end


class Grid
  
  def initialize(size, robot=nil, width_factor=1, log_output=false)
    @size=size
    @robot=robot
    @items = {}
    @width_factor=width_factor
    @log_output=log_output
    # puts "=============="
    # puts "NEW GRID: #{@size}, #{@width_factor}"
    # puts self
  end
  
  def self.parse(map_input, width_factor=1, log_output=false)
    parts = map_input.split("\n\n")
    map = parts[0]
    instructions = parts[1]
    
    rows = map.split("\n")
    size = Coordinates.new(rows[0].length * width_factor, rows.length)
    grid = Grid.new(size, nil, width_factor, log_output)

    rows.each.with_index do |row, rowIdx|
      row.split("").each.with_index do |cell, cellIdx|
        case cell
        when "#"
          wall = Wall.new(Coordinates.new(cellIdx, rowIdx), width_factor)
          wall.add_to_grid(grid)
        when "@"
          grid.set_robot(Coordinates.new(cellIdx * width_factor, rowIdx))
        when "O"
          box = Box.new(Coordinates.new(cellIdx, rowIdx), width_factor)
          box.add_to_grid(grid)
        end
      end
    end

    if log_output then
      puts "====="
      puts "NEW GRID, #{size} x #{width_factor}"
      puts grid
    end
    if instructions then      
      grid.move_robot(instructions, log_output)
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
    (!@items.include?(pos)) &&
    pos.within(size)
  end

  def gather_boxes_until_space(start_pos, movement)
    boxes_until_space=Set[]
    item = item_at(start_pos)
    if @log_output then puts "Found #{item} at #{start_pos}, which is free to move: #{item&.free_to_move(self, movement)}" end
    free_to_move = item.nil? || item&.free_to_move(self, movement)
    if !item.nil? && free_to_move then
      if @log_output then puts "   Adding #{item} to #{boxes_until_space}" end
      boxes_until_space.add(item)
      new_positions = item.test_move(movement)
      if @log_output then puts "   new_positions: #{new_positions.join ","}" end
      new_positions.each do |test_pos|
        if @log_output then puts "   Starting to gather from #{test_pos}" end
        next_boxes = gather_boxes_until_space(test_pos, movement)
        if next_boxes.nil? then
          if @log_output then puts "   ...None found" end
          return nil
        end
        if @log_output then puts "   Some found, adding #{next_boxes.size} boxes to set" end
        boxes_until_space = boxes_until_space | next_boxes
    
      end      
    end
    if !free_to_move or !start_pos.within(@size) then
      if @log_output then puts "   returning nil as free #{!free_to_move} or #{start_pos} within map: #{!start_pos.within(@size)}" end
      return nil
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
      boxes_in_way = gather_boxes_until_space(robot.test_move(movement), movement)
      if !boxes_in_way.nil? && !boxes_in_way.empty? then
        @robot = test_new_pos
        boxes_in_way.each do |box|
          box.remove_from_grid(self)
        end
        boxes_in_way.each do |box|
          box.move(movement)
          box.add_to_grid(self)
        end
      end
    end
  end

  def move_robot(instructions, log=false)
    instructions.split("").each do |movement|      
      if log then puts "\nMoving by #{movement}" end
      @old_robot = @robot
      move_robot_single(movement)
      if log then puts self end
    end    
  end

  # part 1 compatibility
  def add_wall(coords)
    wall = Wall.new(coords)
    wall.add_to_grid(self)
  end

  def add_box(coords)
    box = Box.new(coords)
    box.add_to_grid(self)
  end
  # end part 1 compatibility

  def add_item(item_coords)
    @items[item_coords] = item_coords
  end
  def remove_item(item_coords)
    @items.delete(item_coords)
  end

  def item_at(coords)
    @items[coords]&.item
  end

  def is_box_at(coords)
    !!(item_at(coords)&.is_a? Box)
  end

  def is_wall_at(coords)
    !!(item_at(coords)&.is_a? Wall)
  end

  def gps
    total_gps=0
    boxes = @items.select { |k, item | item.item&.is_a? Box }.map { |k, item | item.item }.uniq.each do |box|
      total_gps+=box.gps
    end
    total_gps
  end

  def to_s
    puts size
    for row in 0...size.y
      for col in 0...size.x
        pos = Coordinates.new(col, row)
        item = item_at(pos)
        case item
        when nil
          if (robot == pos) then
            print "@"
          elsif (@old_robot == pos) then
            print "$"
          else
            print "."
          end
        when Box          
          print item.visualise(pos)
        when Wall
          print "#"
        end
      end
      puts ""
    end
  end
end