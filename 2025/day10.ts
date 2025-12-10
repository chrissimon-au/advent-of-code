import {
    Graph,
    shortestPath,
    serializeGraph,
} from 'graph-data-structure';

import { create, all, MathCollection, MathNumericType, BigNumber } from 'mathjs'

const math = create(all, {});

type Button = number[];
type JoltageLevels = MathCollection<MathNumericType>;

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


interface DistanceOption {
    angle: number
    buttonIdx: number
}

interface Node {
    state: JoltageLevels
    alternativeNextSteps: DistanceOption[]
}

function buttonToVector(button: Button, length: number): number[] {
    const vec = Array(length).fill(0);
    button.forEach(joltageIdx => vec[joltageIdx] = 1);
    return vec;
}

//const angles = buttonVectors.map((bv, idx) => [angle(bv, math.subtract(machine.targetJoltageLevels, currentState)), idx])
function angle(bv: MathCollection<MathNumericType>, currentState: MathCollection<MathNumericType>, targetJoltages: MathCollection<MathNumericType>): number {
    const v2 = math.subtract(targetJoltages, currentState);
    return math.acos(math.divide(math.dot(bv, v2), (math.multiply(math.norm(bv), math.norm(v2)))).valueOf() as number) as number;
}

function distance(bv: MathCollection<MathNumericType>, currentState: MathCollection<MathNumericType>, targetJoltages: MathCollection<MathNumericType>) {
    return math.norm(math.subtract(targetJoltages, math.add(currentState, bv))).valueOf() as number
}

const halfPi = math.pi / 2;

function computeNextSteps(buttonVectors: MathCollection<MathNumericType>, currentState: MathCollection<MathNumericType>, targetJoltages: MathCollection<MathNumericType>): DistanceOption[] {
    return (buttonVectors.map((bv, idx) => ({
        angle: angle(bv, currentState, targetJoltages),
        buttonIdx: idx
    })) as DistanceOption[]).sort((d1, d2) => d2.angle - d1.angle);
}

function keyOfState(state: JoltageLevels): string {
    return (state.valueOf() as number[]).join(',');
}

function countPressesToTargetJoltage(machine: Machine): number {
    const numJoltages: number = math.size(machine.targetJoltageLevels)[0];
    const buttonVectors: number[][] = machine.buttons.map(b => buttonToVector(b, numJoltages))
    //console.log("Buttonvectors", buttonVectors);

    const initialState = math.zeros(numJoltages)
    var currentNode: Node = {
        state: initialState,
        alternativeNextSteps: computeNextSteps(buttonVectors, initialState, machine.targetJoltageLevels)
    }

    const consideredStates = {};

    const path = [];

    var iterations = 0;

    while (!math.deepEqual(currentNode.state, machine.targetJoltageLevels)) {
        const log = iterations % 1_000_000 == 0;// || ((iterations - 1) % 5000 == 0);
        if (log) {
            console.log("===========", iterations);
            console.log("press count:", path.length);
            console.log("CurrentState", currentNode.state.valueOf());
            console.log("Next Step Options", currentNode.alternativeNextSteps);
        }

        const nextStep = currentNode.alternativeNextSteps[currentNode.alternativeNextSteps.length - 1];
        currentNode.alternativeNextSteps.pop(); // remove as we're testing this step now
        if (log) { console.log("Selected next step", nextStep); }
        const nextState = math.add(buttonVectors[nextStep.buttonIdx], currentNode.state)

        var tryNextState = true;

        if (keyOfState(nextState) in consideredStates) {
            if (log) { console.log("Already considered", nextState.valueOf()); }
            tryNextState = false;
        } else {
            const withinTargetBox = math.subtract(machine.targetJoltageLevels, nextState).valueOf() as number[];
            if (log) { console.log("Within target box", withinTargetBox); }
            const isBeyondTarget = withinTargetBox.some(n => n < 0);

            if (isBeyondTarget) {
                if (log) { console.log("  Is past target, unwinding"); }
                tryNextState = false;
            }
        }

        if (tryNextState) {
            if (log) { console.log(" trying next state", nextState.valueOf()); }
            path.push(currentNode);
            consideredStates[keyOfState(currentNode.state)] = true;
            currentNode = {
                state: nextState,
                alternativeNextSteps: computeNextSteps(buttonVectors, nextState, machine.targetJoltageLevels)
            }
        } else {
            while (currentNode.alternativeNextSteps.length == 0) {
                if (log) { console.log("  ran out of alternative next steps, so popping back to previous state", nextState.valueOf()); }
                currentNode = path.pop();
            }
        }
        iterations++;
    }
    console.log("Iterations", iterations);
    return path.length;
}

function getButtonPressesForJoltageLevels(line: string) {
    const machine = parseMachine(line);
    //console.log(machine);
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