import gleam/bool
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type Circuit {
  Circuit(wires: List(Wire))
}

pub type Operation {
  And
  Or
  Xor
}

pub type Wire {
  Wire(
    id: String,
    value: Option(Bool),
    reverse_value: Option(Bool),
    sources: List(String),
    operation: Option(Operation),
  )
}

fn parse_wire(wire_input: String) {
  case string.split(wire_input, ": ") {
    [id, value] ->
      Ok(Wire(id, Some(value == "1"), Some(value == "1"), [], None))
    _ -> Error("Unable to parse wire: " <> wire_input)
  }
}

fn string_to_operation(op_input: String) {
  case op_input {
    "AND" -> Some(And)
    "OR" -> Some(Or)
    "XOR" -> Some(Xor)
    _ -> None
  }
}

fn parse_connection_sources(source_input: String) {
  case string.split(source_input, " ") {
    [source1, op, source2] -> Ok(#([source1, source2], string_to_operation(op)))
    _ -> Error("Unable to parse connection sources: " <> source_input)
  }
}

fn parse_connection(connection_input: String) {
  case string.split(connection_input, " -> ") {
    [srcs, id] ->
      parse_connection_sources(srcs)
      |> result.map(fn(parse_result) {
        let #(sources, op) = parse_result
        Wire(id, None, None, sources, op)
      })
    _ -> Error("Unable to parse connection: " <> connection_input)
  }
}

fn parse_wires(wires_input: String, connections_input: String) {
  let initial_wires =
    string.split(wires_input, "\n")
    |> list.map(parse_wire)
    |> result.all

  let undefined_wires =
    string.split(connections_input, "\n")
    |> list.filter(fn(s) { string.length(s) > 0 })
    |> list.map(parse_connection)
    |> result.all

  result.all([initial_wires, undefined_wires])
  |> result.map(list.flatten)
}

fn create_circuit(wires) {
  case wires {
    Ok(wires) -> Ok(Circuit(wires))
    Error(e) -> Error(e)
  }
}

fn compute_wire(circuit: Circuit, w: Wire) {
  let sources =
    w.sources
    |> list.map(wire(circuit, _))
    |> result.all
    |> result.map(fn(l) { list.map(l, fn(wi) { wi.value }) })
  case sources, w.operation {
    Ok([None, _]), _ -> w
    Ok([_, None]), _ -> w
    Ok([s1, s2]), Some(op) ->
      case op {
        And ->
          Wire(
            ..w,
            value: Some(option.unwrap(s1, False) && option.unwrap(s2, False)),
          )
        Or ->
          Wire(
            ..w,
            value: Some(option.unwrap(s1, False) || option.unwrap(s2, False)),
          )
        Xor ->
          Wire(
            ..w,
            value: Some(bool.exclusive_or(
              option.unwrap(s1, False),
              option.unwrap(s2, False),
            )),
          )
      }
    _, _ -> w
  }
}

fn reverse_compute_wire(circuit: Circuit, w: Wire) {
  io.debug("reverse computing")
  io.debug(w)
  let new_value =
    circuit.wires
    |> list.filter(fn(ow) { ow.sources |> list.any(fn(s) { s == w.id }) })
    |> io.debug
    |> list.map(fn(t) {
      let other_source_id =
        t.sources
        |> list.filter(fn(s) { s != w.id })
        |> list.first
        |> result.unwrap("")
        |> io.debug
      wire(circuit, other_source_id)
      |> io.debug
      |> result.map(fn(other_source) {
        case t.reverse_value, t.operation, other_source.reverse_value {
          Some(True), Some(And), _ -> Some(True)
          Some(False), Some(Or), _ -> Some(False)
          Some(True), Some(Or), Some(False) -> Some(True)
          Some(True), Some(Xor), Some(v) -> Some(!v)
          Some(False), Some(Xor), Some(v) -> Some(v)
          _, _, _ -> None
        }
      })
    })
    |> result.all
    |> io.debug
    |> result.map(fn(l) {
      list.first(l)
      |> result.map(fn(f) {
        case list.all(l, fn(c) { c == f }) {
          True -> f
          _ -> None
        }
      })
      |> option.from_result
      |> option.flatten
    })
    |> option.from_result
    |> option.flatten
    |> io.debug
  case new_value {
    Some(v) -> Wire(..w, reverse_value: Some(v))
    _ -> w
  }
}

pub fn parse_circuit(input: String) {
  case string.split(input, "\n\n") {
    [wires_input, connections_input] ->
      create_circuit(parse_wires(wires_input, connections_input))
    _ -> Error("Unable to parse circuit: " <> input)
  }
}

pub fn wire(circuit: Circuit, wire_id: String) {
  list.find(circuit.wires, fn(w) { w.id == wire_id })
}

fn is_complete(circuit: Circuit) {
  circuit.wires
  |> list.all(fn(w) { option.is_some(w.value) })
}

fn is_reverse_complete(circuit: Circuit, next) {
  circuit == next
}

fn wire_int(wire: Wire) {
  wire.id
  |> string.drop_start(1)
  |> int.parse
  |> result.map(int.to_float)
  |> result.map(int.power(2, _))
  |> result.flatten
  |> result.map(float.truncate)
}

fn port_value(circuit: Circuit, port_id: String) {
  circuit.wires
  |> list.filter(fn(w) {
    string.starts_with(w.id, port_id) && option.unwrap(w.value, False)
  })
  |> list.map(wire_int)
  |> result.all
  |> result.map(int.sum)
}

fn compute_wire_if_uncomputed(circuit: Circuit, wire: Wire) {
  case wire.value {
    Some(_) -> wire
    None -> compute_wire(circuit, wire)
  }
}

fn reverse_compute_wire_if_uncomputed(circuit: Circuit, wire: Wire) {
  case wire.reverse_value {
    Some(_) -> wire
    None -> reverse_compute_wire(circuit, wire)
  }
}

fn complete(circuit: Circuit) {
  case is_complete(circuit) {
    True -> circuit
    False ->
      complete(Circuit(
        circuit.wires
        |> list.map(compute_wire_if_uncomputed(circuit, _)),
      ))
  }
}

fn reverse_complete(circuit: Circuit, attempt: Int) {
  io.debug("======================")
  io.debug(
    "reverse completing "
    <> int.to_string(list.length(circuit.wires))
    <> " attempt "
    <> int.to_string(attempt),
  )
  let next =
    Circuit(
      circuit.wires
      |> list.map(reverse_compute_wire_if_uncomputed(circuit, _)),
    )
  case is_reverse_complete(circuit, next) {
    True -> circuit
    False -> reverse_complete(next, attempt + 1)
  }
}

pub fn output(circuit: Circuit) {
  circuit
  |> complete
  |> port_value("z")
}

fn new_value(wire_int: Result(Int, nil), value: Option(Int)) {
  value
  |> option.map(fn(v) {
    wire_int
    |> result.map(fn(n) { int.bitwise_and(n, v) > 0 })
    |> option.from_result
  })
  |> option.flatten
}

fn wire_with_value(wire: Wire, value: Option(Int)) {
  let new_value = wire_int(wire) |> new_value(value)
  Wire(..wire, value: new_value, reverse_value: new_value)
}

fn wire_with_reverse_value(wire: Wire, value: Option(Int)) {
  let new_value = wire_int(wire) |> new_value(value)
  Wire(..wire, reverse_value: new_value)
}

fn with_port_value(circuit: Circuit, port_id: String, value: Option(Int)) {
  Circuit(
    circuit.wires
    |> list.map(fn(w) {
      case string.first(w.id) {
        Ok(letter) if letter == port_id -> wire_with_value(w, value)
        _ -> w
      }
    }),
  )
}

fn with_reverse_port_value(
  circuit: Circuit,
  port_id: String,
  value: Option(Int),
) {
  Circuit(
    circuit.wires
    |> list.map(fn(w) {
      case string.first(w.id) {
        Ok(letter) if letter == port_id -> wire_with_reverse_value(w, value)
        _ -> w
      }
    }),
  )
}

pub fn find_invalid_gates(circuit: Circuit) {
  let completed =
    circuit
    |> complete
  completed.wires
  |> list.filter_map(fn(w) {
    case w.value, w.reverse_value {
      Some(v1), Some(v2) if v1 != v2 -> Ok(w)
      _, _ -> Error("not invalid")
    }
  })
}

type Input {
  Input(x: Int, y: Int)
}

pub fn find_crossed_wires(
  circuit: Circuit,
  expected_computer: fn(Int, Int) -> Int,
) {
  let max_bit =
    circuit.wires
    |> list.filter_map(fn(w) {
      case string.first(w.id) {
        Ok("z") -> Ok(string.drop_start(w.id, 1))
        _ -> Error("")
      }
    })
    |> list.map(int.parse)
    |> result.all
    |> result.map(list.reduce(_, int.max))
    |> result.flatten
    |> result.unwrap(5)

  let binaries =
    list.range(0, max_bit)
    |> list.map(int.to_float)
    |> list.map(int.power(2, _))
    |> list.map(result.map(_, float.truncate))
    |> result.values

  let input_pairs_same =
    binaries
    |> list.map(fn(i) { Input(i, i) })
  let input_pairs_double =
    binaries
    |> list.map(fn(i) { Input(i, i * 2) })
  let input_pairs_quad =
    binaries
    |> list.map(fn(i) { Input(i, i * 4) })

  input_pairs_same
  |> list.append(input_pairs_double)
  |> list.append(input_pairs_quad)
  |> list.map(fn(inputs) {
    io.debug(inputs)
    circuit
    |> with_port_value("x", Some(inputs.x))
    |> with_port_value("y", Some(inputs.y))
    |> with_reverse_port_value("z", Some(expected_computer(inputs.x, inputs.y)))
    |> complete
    |> reverse_complete(0)
    |> find_invalid_gates
    |> list.map(fn(w) { w.id })
  })
  |> list.flatten
  |> list.sort(string.compare)
  |> list.unique
  |> string.join(",")
  |> Ok
}
