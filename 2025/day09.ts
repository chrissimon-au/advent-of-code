import geometric from 'geometric'

interface Point {
    x: number
    y: number
}

interface Rectangle {
    p1: Point
    p2: Point
}

type Polygon = [number, number][]

function getPointsList(input: string): Point[] {
    const lines = input.split('\n');
    return lines
        .map(l => l.split(','))
        .map(coords => coords.map(x => parseInt(x)))
        .map(([x, y]) => ({ x, y }));
}

function toPolygon(r: Rectangle): Polygon {
    const poly: Polygon = [
        [r.p1.x, r.p1.y],
        [r.p2.x, r.p1.y],
        [r.p2.x, r.p2.y],
        [r.p1.x, r.p2.y],
    ];
    //console.log(r, poly);
    return poly;
}

function area(r: Rectangle): number {
    return (Math.abs(r.p2.x - r.p1.x) + 1) * (Math.abs(r.p2.y - r.p1.y) + 1);
}

function getRectanglesByLargestFirst(points: Point[]): Rectangle[] {
    return points.flatMap(
        (p1, i) => points.slice(i + 1).map(p2 => {
            return { p1, p2 };
        })
    ).sort((r1, r2) => area(r2) - area(r1));
}


export function part1(input: string) {
    const points = getPointsList(input);
    const rectangles = getRectanglesByLargestFirst(points);
    return area(rectangles[0]);
}

function isRectangleInsidePolygon(rectangle: Rectangle, polygon: Polygon): boolean {
    let rectPoly = geometric.polygonScale(toPolygon(rectangle), .999999)
    //console.log("Checking if", rectPoly, "of area", area(rectangle), "is inside", polygon);
    const inside = geometric.polygonInPolygon(rectPoly, polygon);
    //console.log("    = ", inside);
    return inside;
}

function getPolygon(ps: Point[]): Polygon {
    return ps.map(p => [p.x, p.y])
}

export function part2(input: string) {
    const points = getPointsList(input);
    const polygon = getPolygon(points);

    const rectangles = getRectanglesByLargestFirst(points);
    //console.log(rectangles);
    const firstRectangleInPolygon =
        rectangles
            .find(r => isRectangleInsidePolygon(r, polygon))
    if (!!!firstRectangleInPolygon) {
        return -1;
    }
    return area(firstRectangleInPolygon);
}