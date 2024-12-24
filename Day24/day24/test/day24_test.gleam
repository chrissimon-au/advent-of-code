import day24
import glacier
import glacier/should

pub fn main() {
  glacier.main()
}

pub fn wire_can_carry_value_test() {
  let wire =
    day24.create_circuit("x00: 1")
    |> should.be_ok()
    |> day24.wire("x00")
    |> should.be_ok()
  wire.value |> should.be_true
}

pub fn wire_can_carry_false_value_test() {
  let wire =
    day24.create_circuit("y00: 0")
    |> should.be_ok()
    |> day24.wire("y00")
    |> should.be_ok()
  wire.value |> should.be_false
}
