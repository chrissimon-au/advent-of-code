import day24
import glacier
import glacier/should

pub fn main() {
  glacier.main()
}

pub fn wire_can_carry_value_test() {
  let circuit = day24.create_circuit("x00: 1")
  day24.wire(circuit, "x00").value
  |> should.be_true
}
