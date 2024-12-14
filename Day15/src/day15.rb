require 'test/unit'

def check
    true
end


class MyTest < Test::Unit::TestCase
  # def setup
  # end

  # def teardown
  # end

  def test_check
    assert(check())
  end
end