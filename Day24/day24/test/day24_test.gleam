import day24
import glacier
import glacier/should

pub fn main() {
  glacier.main()
}

pub fn wire_can_carry_value_test() {
  let wire =
    day24.create_circuit("x00: 1")
    |> day24.wire("x00")
  wire.value |> should.be_true
}
