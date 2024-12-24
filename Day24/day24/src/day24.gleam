import gleam/list
import gleam/result
import gleam/string

pub type Circuit {
  Circuit(wires: List(Wire))
}

pub type Wire {
  Wire(id: String, value: Bool)
}

fn parse_wire(wire_input: String) {
  case string.split(wire_input, ": ") {
    [id, value] -> Ok(Wire(id, value == "1"))
    _ -> Error("Unable to parse wire: " <> wire_input)
  }
}

fn parse_wires(wires_input: String) {
  string.split(wires_input, "\n")
  |> list.map(parse_wire)
  |> result.all
}

fn create_circuit(wires) {
  case wires {
    Ok(wires) -> Ok(Circuit(wires))
    Error(e) -> Error(e)
  }
}

pub fn parse_circuit(input: String) {
  create_circuit(parse_wires(input))
}

pub fn wire(circuit: Circuit, wire_id: String) {
  list.find(circuit.wires, fn(w) { w.id == wire_id })
}
