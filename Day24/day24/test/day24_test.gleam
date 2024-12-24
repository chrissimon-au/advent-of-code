import day24
import glacier
import glacier/should

pub fn main() {
  glacier.main()
}

pub fn wire_can_carry_value_test() {
  let wire =
    day24.parse_circuit("x00: 1")
    |> should.be_ok()
    |> day24.wire("x00")
    |> should.be_ok()
  wire.value
  |> should.be_some
  |> should.be_true
}

pub fn wire_can_carry_false_value_test() {
  let wire =
    day24.parse_circuit("y00: 0")
    |> should.be_ok()
    |> day24.wire("y00")
    |> should.be_ok()
  wire.value
  |> should.be_some
  |> should.be_false
}

pub fn circuit_can_have_many_wires_test() {
  let circuit =
    day24.parse_circuit("y00: 1\nx00: 0")
    |> should.be_ok()

  let wire =
    circuit
    |> day24.wire("y00")
    |> should.be_ok()

  wire.value
  |> should.be_some
  |> should.be_true

  let wire =
    circuit
    |> day24.wire("x00")
    |> should.be_ok()

  wire.value
  |> should.be_some
  |> should.be_false
}

pub fn circuit_can_have_connections_test() {
  let circuit =
    day24.parse_circuit("y00: 1\nx00: 0\n\nx00 AND y00 -> z00")
    |> should.be_ok()

  let wire =
    circuit
    |> day24.wire("z00")
    |> should.be_ok()

  wire.value
  |> should.be_none
}
