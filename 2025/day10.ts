import {
    Graph,
    shortestPath,
    serializeGraph,
} from 'graph-data-structure';

import {
    mat,
    vec,
    prettyPrint,
    solve,
    Vector,
    FloatMatrix,
    calculateLinearLeastSquares,
    calculateGeneralLeastSquares,
    calculateSingularValueDecomposition,
    UnderdeterminedSolution,
    ApproximationFunctionTemplate
} from '@josh-brown/vector';

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

function vectorCost(v: Vector) {
    return v.toArray().reduce((acc, v) => acc + v);
}

const matrixBuilder = FloatMatrix.builder()

function getPressesToTargetJoltage(machine: Machine): number {

    const buttonMatrix = mat(machine.buttons.map(b => buttonToVector(b, machine.targetJoltageLevels.length))).transpose();
    console.log("buttons:");
    console.log(prettyPrint(buttonMatrix));

    const targetJoltages = vec(machine.targetJoltageLevels);
    console.log("Target:");
    console.log(prettyPrint(targetJoltages));

    const initialSln = solve(buttonMatrix, targetJoltages) as UnderdeterminedSolution<number>;
    console.log("initial solution");
    console.log(prettyPrint(initialSln.solution));

    const columns = buttonMatrix.getColumnVectors();
    const linearData = matrixBuilder.fromColumnVectors([...columns, targetJoltages]).getRowVectors();
    //const res = calculateLinearLeastSquares(linearData.getRowVectors());

    const ops = linearData[0].ops();
    const linearFunctionTemplate: ApproximationFunctionTemplate<number> = (coefficients) => {
        console.log(" new coefficients:")
        console.log(prettyPrint(coefficients));
        return (input: Vector<number>) => {
            console.log("   new input");
            console.log(prettyPrint(input));
            let value = 0; // constant term
            for (let i = 0; i < coefficients.getDimension(); i++) {
                console.log(coefficients.getEntry(i), input.getEntry(i))
                const newTerm = ops.multiply(coefficients.getEntry(i), input.getEntry(i));
                value = ops.add(value, newTerm);
            }
            return value;
        };
    };

    const res = calculateGeneralLeastSquares(linearData, linearFunctionTemplate, linearData[0].getDimension() - 1);
    console.log("linear regression solution");
    console.log(prettyPrint(res.coefficients));
    let args = res.coefficients.toArray().map(x => Math.round(x))
    console.log("  after rounding");
    console.log(args);
    return args.reduce((acc, i) => acc + i);
}

function getButtonPressesForJoltageLevels(line: string) {
    const machine = parseMachine(line);
    //console.log(machine);
    return getPressesToTargetJoltage(machine);
}

export function part1(input: string) {
    const machineLines = input.split('\n');
    return machineLines.reduce((acc, machineLine) => acc + getButtonPressesForLightState(machineLine), 0);
}

export function part2(input: string) {
    const machineLines = input.split('\n');
    return machineLines.reduce((acc, machineLine) => acc + getButtonPressesForJoltageLevels(machineLine), 0);
}