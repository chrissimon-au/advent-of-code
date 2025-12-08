interface Point {
    id: number
    x: number
    y: number
    z: number
}

interface PointPair {
    p1: Point
    p2: Point
}

interface Connection {
    pair: PointPair
    distance: number
}

function distance(pair: PointPair): number {
    return Math.abs(Math.sqrt(Math.pow(pair.p1.x - pair.p2.x, 2) + Math.pow(pair.p1.y - pair.p2.y, 2) + Math.pow(pair.p1.z - pair.p2.z, 2)));
}

function getPointsList(input: string) {
    const lines = input.split('\n');
    return lines
        .map(l => l.split(','))
        .map(coords => coords.map(x => parseInt(x)))
        .map(([x, y, z], id) => ({ id, x, y, z }));
}

function getConnections(boxes: Point[]) {
    return boxes.flatMap(
        (p1, i) => boxes.slice(i + 1).map(p2 => {
            const pair = { p1, p2 };
            return {
                pair,
                distance: distance(pair)
            }
        })
    ).sort((a, b) => a.distance - b.distance);
}


export function part1(input: string, numConnections: number, numCircuits: number) {
    const boxes = getPointsList(input);
    const connections = getConnections(boxes);
    const boxCircuits = {};
    const circuits = {};
    var nextCircuitId = 0;
    for (const connection of connections.slice(0, numConnections)) {
        //console.log("Evaluating connection:", connection);
        if (typeof boxCircuits[connection.pair.p1.id] == 'undefined' && typeof boxCircuits[connection.pair.p2.id] == 'undefined') {
            //console.log("Adding to new circuit");
            boxCircuits[connection.pair.p1.id] = nextCircuitId;
            boxCircuits[connection.pair.p2.id] = nextCircuitId;
            circuits[nextCircuitId] = [connection.pair.p1.id, connection.pair.p2.id];
            //console.log("boxCircuits", boxCircuits);
            //console.log("circuits", circuits);
            nextCircuitId++;
        } else if (typeof boxCircuits[connection.pair.p1.id] == 'undefined') {
            //console.log("Adding p1 to circuit of p2")
            boxCircuits[connection.pair.p1.id] = boxCircuits[connection.pair.p2.id];
            circuits[boxCircuits[connection.pair.p1.id]].push(connection.pair.p1.id);
            //console.log("boxCircuits", boxCircuits);
            //console.log("circuits", circuits);
        } else if (typeof boxCircuits[connection.pair.p2.id] == 'undefined') {
            //console.log("Adding p2 to circuit of p1")
            boxCircuits[connection.pair.p2.id] = boxCircuits[connection.pair.p1.id];
            circuits[boxCircuits[connection.pair.p1.id]].push(connection.pair.p2.id);
            //console.log("boxCircuits", boxCircuits);
            //console.log("circuits", circuits);
        } else if (boxCircuits[connection.pair.p1.id] == boxCircuits[connection.pair.p2.id]) {
            //console.log("Nothing to do, p1 and p2 already in the same circuit")
        } else {
            //console.log("Merging circuits", boxCircuits[connection.pair.p1.id], boxCircuits[connection.pair.p2.id]);
            const targetCircuitId = boxCircuits[connection.pair.p1.id];
            const mergingCircuitId = boxCircuits[connection.pair.p2.id];
            const mergingCircuitBoxes = circuits[mergingCircuitId];
            circuits[targetCircuitId] = (circuits[targetCircuitId] as number[]).concat(mergingCircuitBoxes);
            delete circuits[mergingCircuitId];
            mergingCircuitBoxes.forEach(bId => boxCircuits[bId] = targetCircuitId);
            //console.log("boxCircuits", boxCircuits);
            //console.log("circuits", circuits);
        }
    }
    const relevantCircuitSizes = Object.values(circuits).map((c: number[]) => c.length).sort((a: number, b: number) => b - a).slice(0, numCircuits)
    //console.log("Relevant circuit sizes", relevantCircuitSizes);
    return relevantCircuitSizes.reduce((acc: number, i: number) => acc * i, 1)
}

export function part2(input: string) {
    const boxes = getPointsList(input);
    const numBoxes = boxes.length;
    const connections = getConnections(boxes);
    const boxCircuits = {};
    const circuits = {};
    var nextCircuitId = 0;
    for (const connection of connections) {
        //console.log("Evaluating connection:", connection);
        if (typeof boxCircuits[connection.pair.p1.id] == 'undefined' && typeof boxCircuits[connection.pair.p2.id] == 'undefined') {
            //console.log("Adding to new circuit");
            boxCircuits[connection.pair.p1.id] = nextCircuitId;
            boxCircuits[connection.pair.p2.id] = nextCircuitId;
            circuits[nextCircuitId] = [connection.pair.p1.id, connection.pair.p2.id];
            //console.log("boxCircuits", boxCircuits);
            //console.log("circuits", circuits);
            nextCircuitId++;
        } else if (typeof boxCircuits[connection.pair.p1.id] == 'undefined') {
            //console.log("Adding p1 to circuit of p2")
            boxCircuits[connection.pair.p1.id] = boxCircuits[connection.pair.p2.id];
            circuits[boxCircuits[connection.pair.p1.id]].push(connection.pair.p1.id);
            //console.log("boxCircuits", boxCircuits);
            //console.log("circuits", circuits);
        } else if (typeof boxCircuits[connection.pair.p2.id] == 'undefined') {
            //console.log("Adding p2 to circuit of p1")
            boxCircuits[connection.pair.p2.id] = boxCircuits[connection.pair.p1.id];
            circuits[boxCircuits[connection.pair.p1.id]].push(connection.pair.p2.id);
            //console.log("boxCircuits", boxCircuits);
            //console.log("circuits", circuits);
        } else if (boxCircuits[connection.pair.p1.id] == boxCircuits[connection.pair.p2.id]) {
            //console.log("Nothing to do, p1 and p2 already in the same circuit")
        } else {
            //console.log("Merging circuits", boxCircuits[connection.pair.p1.id], boxCircuits[connection.pair.p2.id]);
            const targetCircuitId = boxCircuits[connection.pair.p1.id];
            const mergingCircuitId = boxCircuits[connection.pair.p2.id];
            const mergingCircuitBoxes = circuits[mergingCircuitId];
            circuits[targetCircuitId] = (circuits[targetCircuitId] as number[]).concat(mergingCircuitBoxes);
            delete circuits[mergingCircuitId];
            mergingCircuitBoxes.forEach(bId => boxCircuits[bId] = targetCircuitId);
            //console.log("boxCircuits", boxCircuits);
            //console.log("circuits", circuits);
        }

        if (Object.keys(boxCircuits).length >= numBoxes && Object.keys(circuits).length == 1) {
            return connection.pair.p1.x * connection.pair.p2.x;
        }
    }
}