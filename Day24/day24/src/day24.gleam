import gleam/list
import gleam/string

pub type Circuit {
  Circuit(wires: List(Wire))
}

pub type Wire {
  Wire(id: String, value: Bool)
}

pub fn create_circuit(input: String) {
  case string.split(input, ": ") {
    [] -> Error("Unable to parse - empty string")
    [_] -> Error("Unable to parse - only one field")
    [id, value] -> Ok(Circuit([Wire(id, value == "1")]))
    _ -> Error("Unable to parse - too many values")
  }
}

pub fn wire(circuit: Circuit, wire_id: String) {
  list.find(circuit.wires, fn(w) { w.id == wire_id })
}
