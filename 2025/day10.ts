import {
    Graph,
    shortestPath,
    serializeGraph,
} from 'graph-data-structure';

type Button = number[];

interface Machine {
    targetLightState: number
    buttons: Button[]
}

function parseLightState(input: string) {
    return input
        .slice(1, input.length - 1)
        .split('')
        .map((char, i) => char == '#' ? Math.pow(2, i) : 0)
        .reduce((acc, light) => acc + light);
}

function parseButtons(buttonTexts: string[]) {
    return buttonTexts.map(buttonText => buttonText.slice(1, buttonText.length - 1).split(',').map(buttonLight => parseInt(buttonLight)));
}

function parseMachine(line: string) {
    const parts = line.split(/ /);
    const lights = parts[0];
    const buttons = parts.slice(1, parts.length - 1);
    return {
        targetLightState: parseLightState(lights),
        buttons: parseButtons(buttons)
    };
}

function getNextState(state: string, button: Button) {
    let stateNum = parseInt(state);
    button.forEach(light => stateNum = stateNum ^ Math.pow(2, light))
    return stateNum.toString();
}

function addNodeEdges(graph: Graph, machine: Machine, state: string, processedStates: object) {
    processedStates[state] = true;
    machine.buttons.forEach(button => {
        const nextState = getNextState(state, button);
        if (!graph.hasEdge(state, nextState)) {
            graph.addEdge(state, nextState);
        }
        if (!(nextState in processedStates)) {
            addNodeEdges(graph, machine, nextState, processedStates);
        }
    });
}

function getGraphFromMachine(machine: Machine) {
    const graph = new Graph();
    addNodeEdges(graph, machine, '0', {});
    return graph;
}

function getSmallestNumberOfButtonPresses(line: string) {
    const machine = parseMachine(line);
    const graph = getGraphFromMachine(machine);
    return shortestPath(graph, '0', machine.targetLightState.toString()).weight;
}

export function part1(input: string) {
    const machineLines = input.split('\n');
    return machineLines.reduce((acc, machineLine) => acc + getSmallestNumberOfButtonPresses(machineLine), 0);    
}

export function part2(input: string) {

}