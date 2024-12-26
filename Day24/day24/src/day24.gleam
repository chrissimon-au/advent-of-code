import gleam/bool
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regexp.{type Match}
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
    normalized_id: Option(String),
    value: Option(Bool),
    reverse_value: Option(Bool),
    sources: List(String),
    operation: Option(Operation),
  )
}

fn parse_wire(wire_input: String) {
  case string.split(wire_input, ": ") {
    [id, value] ->
      Ok(Wire(id, Some(id), Some(value == "1"), Some(value == "1"), [], None))
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
        let normalized_id = case id {
          "z" <> _ -> Some(id)
          _ -> None
        }
        Wire(id, normalized_id, None, None, sources, op)
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

pub fn output(circuit: Circuit) {
  circuit
  |> complete
  |> port_value("z")
}

fn normalize_wire_id_with(wire: Wire, nid: String) {
  Wire(..wire, normalized_id: Some(nid))
}

fn format(i: Int, n: Int) {
  i
  |> int.to_string
  |> string.pad_start(n, "0")
}

fn match_sources(
  circuit: Circuit,
  srcs: List(String),
  gp1: String,
  gp2: String,
  n_fn: fn(Int) -> Int,
) {
  let assert Ok(source_match) = regexp.from_string("(\\D+)(\\d+)")
  let matches =
    srcs
    |> list.map(wire(circuit, _))
    |> list.map(fn(wire) {
      let assert Ok(w) = wire
      case w.normalized_id {
        Some(nid) -> nid
        None -> w.id
      }
    })
    |> list.map(regexp.scan(source_match, _))
    |> list.flatten
  case matches {
    [
      regexp.Match(_, [Some(p1), Some(n1)]),
      regexp.Match(_, [Some(p2), Some(n2)]),
    ] -> {
      let assert Ok(n1i) = int.parse(n1)
      let assert Ok(n2i) = int.parse(n2)
      case [p1, p2] == [gp1, gp2] && n_fn(n1i) == n2i {
        True -> Some(n1)
        False ->
          case [p1, p2] == [gp2, gp1] && n_fn(n2i) == n1i {
            True -> Some(n2)
            False -> None
          }
      }
    }
    _ -> None
  }
}

fn id(x: Int) {
  x
}

fn sub1(x: Int) {
  x - 1
}

fn get_reversed_normalized_wire_id(wire: Wire, circuit: Circuit) {
  let new_wire =
    circuit.wires
    |> list.filter(fn(w) { list.contains(w.sources, wire.id) })
    |> list.map(fn(t) {
      case t {
        Wire("z" <> n, _, _, _, [s1, _], Some(Xor)) if s1 == wire.id -> {
          let assert Ok(ni) = int.parse(n)
          Some(normalize_wire_id_with(wire, "c" <> format(ni - 1, 2)))
        }
        Wire("z" <> n, _, _, _, [_, s2], Some(Xor)) if s2 == wire.id -> {
          let assert Ok(ni) = int.parse(n)
          Some(normalize_wire_id_with(wire, "c" <> format(ni - 1, 2)))
        }

        _ -> None
      }
    })
    |> option.values
    |> list.filter(fn(w) { option.is_some(w.normalized_id) })
    |> list.sort(fn(w1, w2) { string.compare(w1.id, w2.id) })
    |> list.unique

  case new_wire {
    [_, _, ..] -> {
      io.debug("Found a mismatch")
      new_wire |> io.debug
    }
    _ -> new_wire
  }
  case new_wire |> list.first {
    Ok(n) -> n
    Error(_) -> wire
  }
}

fn is_normalized_mismatch(wire: Wire, circuit: Circuit) {
  let n_wire = get_reversed_normalized_wire_id(wire, circuit)
  case wire.normalized_id, n_wire.normalized_id {
    Some("s'" <> n1), Some("c" <> n2) -> {
      let assert Ok(ni1) = int.parse(n1)
      let assert Ok(ni2) = int.parse(n2)
      case ni1 - ni2 {
        1 -> False
        _ -> {
          io.debug("found a mismatch")
          n_wire |> io.debug
          wire |> io.debug
          True
        }
      }
    }
    Some("x00"), Some("c-1") -> False
    Some("y00"), Some("c-1") -> False
    Some(id1), Some(id2) if id1 == id2 -> False
    _, _ -> {
      io.debug("found a mismatch")
      n_wire |> io.debug
      wire |> io.debug
      True
    }
  }
}

fn reverse_normalize_wire_id(wire: Wire, circuit: Circuit) {
  case wire.normalized_id {
    Some(_) -> wire
    None -> {
      get_reversed_normalized_wire_id(wire, circuit)
    }
  }
}

fn forward_normalize_wire_id(wire: Wire, circuit: Circuit) {
  case wire {
    Wire(_, None, _, _, srcs, Some(Xor)) ->
      case match_sources(circuit, srcs, "x", "y", id) {
        Some(n) -> wire |> normalize_wire_id_with("s'" <> n)
        None -> wire
      }

    Wire(_, None, _, _, srcs, Some(And)) ->
      case match_sources(circuit, srcs, "x", "y", id) {
        Some(n) -> wire |> normalize_wire_id_with("c'" <> n)
        None ->
          case match_sources(circuit, srcs, "s'", "c", sub1) {
            Some(n) -> wire |> normalize_wire_id_with("c''" <> n)
            None -> wire
          }
      }

    Wire(_, None, _, _, srcs, Some(Or)) -> {
      case match_sources(circuit, srcs, "c'", "c''", id) {
        Some(n) -> wire |> normalize_wire_id_with("c" <> n)
        None -> wire
      }
    }
    _ -> wire
  }
}

fn normalize_wire_id(circuit: Circuit, wire: Wire) {
  wire
  |> forward_normalize_wire_id(circuit)
  |> reverse_normalize_wire_id(circuit)
}

// fn all_normalized(circuit: Circuit) {
//   circuit.wires
//   |> list.all(fn(w) { option.is_some(w.normalized_id) })
// }

fn normalize_circuit_ids(circuit: Circuit) {
  let next = Circuit(circuit.wires |> list.map(normalize_wire_id(circuit, _)))
  case circuit == next {
    True -> circuit
    False -> normalize_circuit_ids(next)
  }
}

pub fn analyse_adder_for_crossed_wires(circuit: Circuit) {
  let normalized =
    circuit
    |> normalize_circuit_ids

  normalized.wires
  |> list.filter(fn(w) {
    option.is_none(w.normalized_id) || is_normalized_mismatch(w, normalized)
  })
  |> list.map(fn(w) { w.id })
  |> list.sort(string.compare)
  |> list.unique
  |> string.join(",")
  |> Ok
}
