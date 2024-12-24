import day24
import glacier
import glacier/should
import gleam/int
import simplifile

pub fn main() {
  glacier.main()
}

pub fn wire_can_carry_value_test() {
  let wire =
    day24.parse_circuit("x00: 1\n\n")
    |> should.be_ok()
    |> day24.wire("x00")
    |> should.be_ok()
  wire.value
  |> should.be_some
  |> should.be_true
}

pub fn wire_can_carry_false_value_test() {
  let wire =
    day24.parse_circuit("y00: 0\n\n")
    |> should.be_ok()
    |> day24.wire("y00")
    |> should.be_ok()
  wire.value
  |> should.be_some
  |> should.be_false
}

pub fn circuit_can_have_many_wires_test() {
  let circuit =
    day24.parse_circuit("y00: 1\nx00: 0\n\n")
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
    |> should.be_ok

  wire.value
  |> should.be_none

  wire.sources
  |> should.equal(["x00", "y00"])

  wire.operation
  |> should.be_some
  |> should.equal(day24.And)
}

pub fn circuit_can_compute_result_test() {
  day24.parse_circuit(
    "x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02",
  )
  |> should.be_ok
  |> day24.output
  |> should.be_ok
  |> should.equal(4)
}

fn load_circuit_from_file(base: String) {
  simplifile.read("../" <> base <> "data.txt")
  |> should.be_ok
  |> day24.parse_circuit
  |> should.be_ok
}

fn circuit_can_compute_result_from_file(base: String) {
  let answer =
    simplifile.read("../" <> base <> "data.answer.txt")
    |> should.be_ok
    |> int.parse
    |> should.be_ok

  load_circuit_from_file(base)
  |> day24.output
  |> should.be_ok
  |> should.equal(answer)
}

pub fn can_compute_aoc_sample_test() {
  circuit_can_compute_result_from_file("sample")
}

pub fn can_compute_aoc_test_test() {
  circuit_can_compute_result_from_file("test")
}

pub fn check_inputs_and_output_test() {
  let circuit = load_circuit_from_file("sample2")

  circuit
  |> day24.find_crossed_wires(40)
  |> should.be_ok
  |> should.equal([])
}
