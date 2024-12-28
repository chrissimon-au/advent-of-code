require 'test/unit'
require './day15'

class MyTest < Test::Unit::TestCase
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

    def test_box_movement_blocked_by_edge
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

    def test_process_instructions
        map_input = <<~EOS
                    ########
                    #..O.O.#
                    ##@.O..#
                    #...O..#
                    #.#.O..#
                    #...O..#
                    #......#
                    ########

                    <^^>>>vv<v>>v<<
                    EOS
        grid = Grid.parse(map_input)

        assert_equal(2028, grid.gps)
    end

    def test_aoc_sample
        map_input = File.read("../sampledata.txt")
        expected = Integer(File.read("../sampledata.answer.txt"))

        grid = Grid.parse(map_input)

        assert_equal(expected, grid.gps)
    end

    def test_aoc_test
        map_input = File.read("../testdata.txt")
        expected = Integer(File.read("../testdata.answer.txt"))

        grid = Grid.parse(map_input)

        assert_equal(expected, grid.gps)
    end
end  