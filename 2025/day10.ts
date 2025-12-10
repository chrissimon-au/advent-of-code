import {
    Graph,
    shortestPath,
    serializeGraph,
} from 'graph-data-structure';

import Queue from 'yocto-queue';

type Button = number[];
type JoltageLevels = number[];

interface Machine {
    targetLightState: number
    targetJoltageLevels: JoltageLevels
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

function parseJoltageLevels(joltageText: string) {
    return joltageText.slice(1, joltageText.length - 1).split(',').map(level => parseInt(level));
}

function parseMachine(line: string): Machine {
    const parts = line.split(/ /);
    const lights = parts[0];
    const buttons = parts.slice(1, parts.length - 1);
    const joltageLevels = parts[parts.length - 1];
    return {
        targetLightState: parseLightState(lights),
        buttons: parseButtons(buttons),
        targetJoltageLevels: parseJoltageLevels(joltageLevels)
    };
}

function getNextState(state: string, button: Button) {
    let stateNum = parseInt(state);
    button.forEach(light => stateNum = stateNum ^ Math.pow(2, light))
    return stateNum.toString();
}

function addLightsNodeEdges(graph: Graph, machine: Machine, state: string, processedStates: object) {
    processedStates[state] = true;
    machine.buttons.forEach(button => {
        const nextState = getNextState(state, button);
        if (!graph.hasEdge(state, nextState)) {
            graph.addEdge(state, nextState);
        }
        if (!(nextState in processedStates)) {
            addLightsNodeEdges(graph, machine, nextState, processedStates);
        }
    });
}

function getLightsGraphFromMachine(machine: Machine) {
    const graph = new Graph();
    addLightsNodeEdges(graph, machine, '0', {});
    return graph;
}

function getButtonPressesForLightState(line: string) {
    const machine = parseMachine(line);
    const graph = getLightsGraphFromMachine(machine);
    return shortestPath(graph, '0', machine.targetLightState.toString()).weight;
}

interface Node {
    state: number[]
    stepCount: number
}

/*
https://en.wikipedia.org/wiki/Breadth-first_search
 procedure BFS(G, root) is
 2      let Q be a queue
 3      label root as explored
 4      Q.enqueue(root)
 5      while Q is not empty do
 6          v := Q.dequeue()
 7          if v is the goal then
 8              return v
 9          for all edges from v to w in G.adjacentEdges(v) do
10              if w is not labeled as explored then
11                  label w as explored
12                  w.parent := v
13                  Q.enqueue(w)
*/

function joltagesEqual(j1: JoltageLevels, j2: JoltageLevels) {
    console.log("  Is", j1, "equal to", j2);
    const areEqual = j1.every((v, i) => v == j2[i]);
    console.log("   ", areEqual);
    return areEqual;
}

function computeNextJoltageLevels(state: JoltageLevels, button: Button) {
    const nextState = [...state];
    button.forEach(buttonJoltage => nextState[buttonJoltage]++)
    console.log("getting nextstate from", state, "with", button, "and it's", nextState);
    if (nextState.length != state.length) {
        throw Error("Incorrect state " + JSON.stringify(nextState) + ", from applying " + JSON.stringify(button) + " to " + JSON.stringify(state));
    }
    return nextState;
}

function isStateOnPathToTarget(state: JoltageLevels, target: JoltageLevels) {
    console.log("  Is state", state, "on path to", target, "?");
    const isOnPath = state.every((jl, i) => jl <= target[i]);
    console.log("   ", isOnPath);
    return isOnPath;
}

function countPressesToTargetJoltage(machine: Machine) {
    const queue = new Queue();
    const initialNode = {
        state: Array(machine.targetJoltageLevels.length).fill(0),
        stepCount: 0
    }
    queue.enqueue(initialNode);
    while (queue.size > 0) {
        console.log(queue.size);
        const current = queue.dequeue() as Node;
        for (const button of machine.buttons) {
            const nextState = {
                stepCount: current.stepCount + 1,
                state: computeNextJoltageLevels(current.state, button)
            }
            if (joltagesEqual(nextState.state, machine.targetJoltageLevels)) {
                console.log("Found!", nextState);
                return nextState.stepCount;
            }
            if (isStateOnPathToTarget(nextState.state, machine.targetJoltageLevels)) {
                console.log("Enqueing", nextState);
                queue.enqueue(nextState);
            }
        }
    }
    return -1;
}


function getButtonPressesForJoltageLevels(line: string) {
    const machine = parseMachine(line);
    console.log(machine);
    return countPressesToTargetJoltage(machine);
}

export function part1(input: string) {
    const machineLines = input.split('\n');
    return machineLines.reduce((acc, machineLine) => acc + getButtonPressesForLightState(machineLine), 0);
}

export function part2(input: string) {
    const machineLines = input.split('\n');
    return machineLines.reduce((acc, machineLine) => acc + getButtonPressesForJoltageLevels(machineLine), 0);
}