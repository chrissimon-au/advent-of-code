import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type Circuit {
  Circuit(wires: List(Wire))
}

pub type Wire {
  Wire(id: String, value: Option(Bool))
}

fn parse_wire(wire_input: String) {
  case string.split(wire_input, ": ") {
    [id, value] -> Ok(Wire(id, Some(value == "1")))
    _ -> Error("Unable to parse wire: " <> wire_input)
  }
}

fn parse_connection(connection_input: String) {
  case string.split(connection_input, " -> ") {
    [_, id] -> Ok(Wire(id, None))
    _ -> Error("Unable to parse connection: " <> connection_input)
  }
}

fn parse_wires(wires_input: String, connections_input: String) {
  let initial_wires =
    string.split(wires_input, "\n")
    |> list.map(parse_wire)
    |> result.all
  
  
  let undefined_wires = string.split(connections_input, "\n")    
    |> list.filter(fn (s) {string.length(s) > 0})
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
