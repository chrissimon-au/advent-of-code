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

  def free_to_move(grid, movement)
    new_coords = test_move(movement)
    new_coords.all? do | coords |
      item = grid.item_at(coords)
      item.nil? || (item.is_a? Box)  
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
    test_movement = Coordinates.new(movement.x*@width,movement.y)    
    new_position(test_movement)
  end

  def move(movement)
    @coords = new_position(movement)
  end

  def gps
    @coords[0].gps
  end

end

class Wall < Item
end


class Grid
  
  def initialize(size, robot=nil, width_factor=1)
    @size=size
    @robot=robot
    @items = {}
    @width_factor=width_factor
    # puts "=============="
    # puts "NEW GRID: #{@size}, #{@width_factor}"
    # puts self
  end
  
  def self.parse(map_input, width_factor=1)
    parts = map_input.split("\n\n")
    map = parts[0]
    instructions = parts[1]
    
    rows = map.split("\n")
    size = Coordinates.new(rows[0].length * width_factor, rows.length)
    grid = Grid.new(size, nil, width_factor)

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
    (!@items.include?(pos)) &&
    pos.within(size)
  end

  def gather_boxes_until_space(movement)
    boxes_until_space=Set[]
    test_pos = robot.test_move(movement)
    item = item_at(test_pos)
    # puts "Found #{item} at #{test_pos}, which is free to move: #{item&.free_to_move(self, movement)}"
    while (!item.nil? && item&.free_to_move(self, movement))
      # puts "Adding #{item} to #{boxes_until_space}"
      boxes_until_space.add(item)
      test_pos = item.test_move(movement)[0]      
      item = item_at(test_pos)
      # puts "Found #{item} at #{test_pos}, which is free to move: #{item&.free_to_move(self, movement)}"
    end
    if !item.nil? or !test_pos.within(@size) then
      return []
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
      if log then puts "Moving by #{movement}" end
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
    boxes = @items.select { |k, item | item.item&.is_a? Box }.each do |k, item|
      total_gps+=item.gps
    end
    total_gps
  end

  def to_s
    puts ""
    puts size
    for row in 0...size.y
      for col in 0...size.x
        pos = Coordinates.new(col, row)
        item = item_at(pos)
        case item
        when nil
          if (robot == pos) then
            print "@"
          else
            print "."
          end
        when Box
          print "O"
        when Wall
          print "#"        
        end
      end
      puts ""
    end
  end
end