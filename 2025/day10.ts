import {
    Graph,
    shortestPath,
    serializeGraph,
} from 'graph-data-structure';

import { loadModule, Model } from 'glpk-ts';

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


function buttonToVector(button: Button, length: number): number[] {
    const buttonVec = Array(length).fill(0);
    button.forEach(joltageIdx => buttonVec[joltageIdx] = 1);
    return buttonVec;
}

function getPressesToTargetJoltage(machine: Machine): number {
    const m = new Model({ sense: 'min' });
    const vars = m.addVars(machine.buttons.length, { lb: 0.0, obj: 1.0, type: "i" });
    machine.targetJoltageLevels.forEach((targetJoltageLevel, jlIdx) =>
        m.addConstr({
            name: `jl${jlIdx}`,
            ub: targetJoltageLevel,
            lb: targetJoltageLevel,
            coeffs: vars.map((v, i) => [v, machine.buttons[i].some(bJl => bJl == jlIdx) ? 1.0 : 0.0]),
        })
    );
    console.log("attempting solve: ", m.intopt({ presolve: true }));        
    console.log(m.solutionMIP)
    const value = /value = (\d+)/g.exec(m.solutionMIP);
    return parseInt(value[1]);
}

function getButtonPressesForJoltageLevels(line: string, idx: number) {
    console.log(`${idx} ===============`);
    console.log(line);
    const machine = parseMachine(line);
    //console.log(machine);
    return getPressesToTargetJoltage(machine);
}

export function part1(input: string) {
    const machineLines = input.split('\n');
    return machineLines.reduce((acc, machineLine) => acc + getButtonPressesForLightState(machineLine), 0);
}

export async function part2(input: string) {
    const machineLines = input.split('\n');
    await loadModule();
    return machineLines.reduce((acc, machineLine, i) => acc + getButtonPressesForJoltageLevels(machineLine, i), 0);
}