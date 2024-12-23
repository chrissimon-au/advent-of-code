import day24
import glacier
import glacier/should

pub fn main() {
  glacier.main()
}

pub fn check_test() {
  day24.check()
  |> should.be_true
}
