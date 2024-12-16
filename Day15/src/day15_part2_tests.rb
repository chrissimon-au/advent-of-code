require 'test/unit'
require './day15'

class MyTest < Test::Unit::TestCase

    def test_double_width
        map_input = <<~EOS
                    ########
                    #..O.O.#
                    ##@.O..#
                    #...O..#
                    #.#.O..#
                    #...O..#
                    #......#
                    ########

                    >>>>
                        EOS
        grid = Grid.parse(map_input, 2)

        assert_equal(16, grid.size.x)
        assert_equal(8, grid.size.y)
        assert(grid.is_wall_at(Coordinates.new(0,1)))
        assert(grid.is_wall_at(Coordinates.new(1,1)))
        assert(grid.is_box_at(Coordinates.new(6,1)))
        assert(grid.is_box_at(Coordinates.new(7,1)))
        assert(grid.is_box_at(Coordinates.new(10,2)))
        assert_equal(8, grid.robot.x)
        assert_equal(2, grid.robot.y)
    end

    def test_double_width_boxes_block_when_mis_aligned
        map_input = <<~EOS
                    ########
                    #......#
                    ##.@O..#
                    #...O..#
                    #.#.O..#
                    #...O..#
                    #......#
                    ########

                    ">>^>v"
                    EOS
        grid = Grid.parse(map_input, 2)

        assert(grid.is_box_at(Coordinates.new(9,3)))
        assert(grid.is_box_at(Coordinates.new(10,3)))
        assert(grid.is_box_at(Coordinates.new(8,6)))
        assert(grid.is_box_at(Coordinates.new(9,6)))
        assert_equal(9, grid.robot.x)
        assert_equal(2, grid.robot.y)
    end

    def test_double_width_boxes_move_multiple_boxes
        map_input = <<~EOS
                    ########
                    #......#
                    ##.@O..#
                    #...OO.#
                    #.#....#
                    #......#
                    #......#
                    ########

                    >>^>v
                    EOS
        grid = Grid.parse(map_input, 2)

        assert(grid.is_box_at(Coordinates.new(9,3)))
        assert(grid.is_box_at(Coordinates.new(10,3)))
        assert(grid.is_box_at(Coordinates.new(8,4)))
        assert(grid.is_box_at(Coordinates.new(9,4)))
        assert(grid.is_box_at(Coordinates.new(10,4)))
        assert(grid.is_box_at(Coordinates.new(11,4)))
        assert_equal(9, grid.robot.x)
        assert_equal(2, grid.robot.y)
    end

    def test_single_gap
        map_input = <<~EOS
                ########
                #O.O@..#
                #......#
                ########

                <<
                EOS
        grid = Grid.parse(map_input, 2)

        assert(grid.is_box_at(Coordinates.new(2,1)))
        assert(grid.is_box_at(Coordinates.new(3,1)))
        assert(grid.is_box_at(Coordinates.new(4,1)))
        assert(grid.is_box_at(Coordinates.new(5,1)))
        assert_equal(6, grid.robot.x)
        assert_equal(1, grid.robot.y)
    end

    def test_wcabus
        map_input = <<~EOS
                    #######
                    #...#.#
                    #.....#
                    #.....#
                    #.....#
                    #.....#
                    #.OOO@#
                    #.OOO.#
                    #..O..#
                    #.....#
                    #.....#
                    #######

                    v<vv<<^^^^^
                    EOS
        grid = Grid.parse(map_input, 2)

        assert_equal(2339, grid.gps)
    end

    def test_corner
        map_input = <<~EOS
                    #######
                    #.....#
                    #.....#
                    #.@O..#
                    #..#O.#
                    #...O.#
                    #..O..#
                    #.....#
                    #######

                    >><vvv>v>^^^
                    EOS

        grid = Grid.parse(map_input, 2, false)

        assert_equal(1430, grid.gps)
    end

    def test_aoc_sample
        map_input = File.read("../sampledata.txt")
        expected = Integer(File.read("../sampledata.answer2.txt"))

        grid = Grid.parse(map_input, 2)

        assert_equal(expected, grid.gps)
    end

    def test_aoc_test
        map_input = File.read("../testdata.txt")
        expected = Integer(File.read("../testdata.answer2.txt"))

        grid = Grid.parse(map_input, 2)

        assert_equal(expected, grid.gps)
    end
end