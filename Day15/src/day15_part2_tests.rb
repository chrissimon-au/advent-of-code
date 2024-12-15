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
                        EOS
        grid = Grid.parse(map_input, true)

        assert_equal(16, grid.size.x)
        assert_equal(8, grid.size.y)
        assert(grid.is_wall_at(Coordinates.new(0,1)))
        assert(grid.is_wall_at(Coordinates.new(1,1)))
        assert(grid.is_box_at(Coordinates.new(6,1)))
        assert(grid.is_box_at(Coordinates.new(7,1)))
        assert_equal(4, grid.robot.x)
        assert_equal(2, grid.robot.y)
    end
end