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
    ones,
    euclideanNorm,
    Vector,
    Matrix,
    FloatMatrix,
    calculateLinearLeastSquares,
    calculateGeneralLeastSquares,
    calculateSingularValueDecomposition,
    UnderdeterminedSolution,
    ApproximationFunctionTemplate,
    LinearSolution,
    SolutionType,
    UniqueSolution,
    FloatVector
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

function isValidSlnVector(v: Vector<number>) {
    return v.toArray().every(a => Math.round(a) >= 0 && Math.abs(Math.round(a) - a) < Number.EPSILON);
}

function isValid(sln: LinearSolution<number>) {
    if (sln.solutionType == SolutionType.OVERDETERMINED) {
        return false;
    }
    return isValidSlnVector(sln.solution);
}

function slnCost(v: Vector<number>) {
    return v.toArray().reduce((a, i) => a + i);
}

function slnError(v: Vector<number>) {
    return -(v.toArray().filter(a => a < 0).reduce((a, i) => a + i, 0));
}

function logVecArr(name: string, m: Matrix<number> | Vector<number>) {
    console.log(name);
    const mp = m instanceof FloatMatrix ?
        prettyPrint(m)
        : (m instanceof FloatVector ? prettyPrint(m) : `${m.toArray()}`);
    console.log(mp);
}

const matrixBuilder = FloatMatrix.builder()
const vectorBuilder = FloatVector.builder()

function findSolutionAtCost(cost: number, targetJoltages: JoltageLevels, verificationMatrix: FloatMatrix, log: boolean): LinearSolution<number> {
    if (log) { console.log("-----------"); }
    if (log) { console.log("Target cost", cost); }
    const verificationTarget = vec([...targetJoltages, cost]);
    if (log) { logVecArr("Verification Target at cost", verificationTarget); }


    var sln: LinearSolution<number> = null;
    try {
        sln = solve(verificationMatrix, verificationTarget);
    } catch (ex) {
        if (log) { console.log("Error attempting solve", ex) }
        return { solutionType: SolutionType.OVERDETERMINED };
    }

    if (isValid(sln)) {
        if (log) { logVecArr("Found Sln on first attempt", (sln as UnderdeterminedSolution<number>).solution) }
        return sln;
    }
    if (sln.solutionType == SolutionType.OVERDETERMINED) {
        if (log) { console.log("Overdetermined, abandoning"); }
        return sln;
    }

    if (log) {
        logVecArr("Not valid first attempt", sln.solution);
    }
    const dims = sln.solution.getDimension();

    const zeroPositions = sln.solution.toArray().map((z, i) => [z, i]).filter(([z, i]) => z < 0).map(([z, i]) => i);
    const zeroRows = zeroPositions.map(zp => vectorBuilder.fromIndexFunction(dims, (i) => zp == i ? 1 : 0));
    const rows = verificationMatrix.getRowVectors();
    const verifyWithZerosMatrix = matrixBuilder.fromRowVectors([...rows, ...zeroRows]);
    const zeroVerificationTarget = vec([...verificationTarget.toArray(), ...Array(zeroPositions.length).fill(0)])

    if (log) {
        logVecArr("VerifywithZerosMatrix", verifyWithZerosMatrix);
        logVecArr("ZeroVerificationTarget", zeroVerificationTarget);
    }

    try {
        const origSln = sln;
        sln = solve(verifyWithZerosMatrix, zeroVerificationTarget);
        if (log) { console.log("Solution found after attempt with zeroes", sln) }
        if (sln.solutionType == SolutionType.OVERDETERMINED) {
            return origSln;
        }
        return sln;
    } catch (ex) {
        if (log) { console.log("Error attempting solve", ex) }
        return sln;
    }
}

function getPressesToTargetJoltage(machine: Machine): number {
    const logAll = false;

    const buttonMatrix = mat(machine.buttons.map(b => buttonToVector(b, machine.targetJoltageLevels.length))).transpose();
    if (logAll) { logVecArr("buttons", buttonMatrix); }

    const targetJoltages = vec(machine.targetJoltageLevels);
    if (logAll) { logVecArr("Target:", targetJoltages); }

    const verificationRow = ones(machine.buttons.length);
    const rows = buttonMatrix.getRowVectors();
    const verificationMatrix = matrixBuilder.fromRowVectors([...rows, verificationRow]);

    // minimum presses is the largest target joltage
    var cost = Math.max(...machine.targetJoltageLevels);
    // maximum presses is the sum of all target joltages
    const MaxCost = machine.targetJoltageLevels.reduce((a, i) => a + i);
    // solution must be in this range.

    const underDeterminedSolutions: (UnderdeterminedSolution<number> | UniqueSolution<number>)[] = [];

    var sln: LinearSolution<number> = findSolutionAtCost(cost, machine.targetJoltageLevels, verificationMatrix, logAll);

    while (!isValid(sln)) {
        const log = logAll;//iterator % 1000 == 0;
        cost++;

        if (cost > MaxCost) {
            if (underDeterminedSolutions.length == 1) {
                console.log();
                logVecArr("Only one underdetermined solution found in range, it must be it", underDeterminedSolutions[0].solution);
                return Math.round(slnCost(underDeterminedSolutions[0].solution));
            }
            if (underDeterminedSolutions.length > 1) {
                const sortedUndeterminedCosts = underDeterminedSolutions.map(s => slnCost(s.solution)).sort();
                //const sortedUndetermined = underDeterminedSolutions.sort((s1, s2) => slnError(s1.solution) - slnError(s2.solution));
                //sortedUndetermined.forEach((s, i) => logVecArr(`undetermined ${i}`, s.solution));
                //const lowestErrorUndetermined = sortedUndetermined[0];
                //throw new Error("need to check handling of multiple undetermineds")
                console.log("Selecting lowest cost from all underdetermined solutions", sortedUndeterminedCosts[0])
                return sortedUndeterminedCosts[0];
            }
            throw new Error(`Cost ${cost} exceeds max cost ${MaxCost} without any underdetermined solutions`)
        }

        if (sln.solutionType == SolutionType.UNDERDETERMINED || sln.solutionType == SolutionType.UNIQUE) {
            underDeterminedSolutions.push(sln);
        }

        sln = findSolutionAtCost(cost, machine.targetJoltageLevels, verificationMatrix, log);


    }
    logVecArr(`Found solution with cost ${cost}`, (sln as UnderdeterminedSolution<number>).solution);
    return cost;
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

export function part2(input: string) {
    const machineLines = input.split('\n');
    return machineLines.reduce((acc, machineLine, i) => acc + getButtonPressesForJoltageLevels(machineLine, i), 0);
}