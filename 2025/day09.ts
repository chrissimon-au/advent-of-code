interface Point {
    id: number
    x: number
    y: number
}

interface PointPair {
    p1: Point
    p2: Point
}

interface Rectangle {
    pair: PointPair
    area: number
}

function getPointsList(input: string) {
    const lines = input.split('\n');
    return lines
        .map(l => l.split(','))
        .map(coords => coords.map(x => parseInt(x)))
        .map(([x, y], id) => ({ id, x, y }));
}

function area(pair: PointPair) {
    return (Math.abs(pair.p2.x - pair.p1.x) + 1) * (Math.abs(pair.p2.y - pair.p1.y) + 1);
}

function getRectangles(boxes: Point[]) {
    return boxes.flatMap(
        (p1, i) => boxes.slice(i + 1).map(p2 => {
            const pair = { p1, p2 };
            return {
                pair,
                area: area(pair)
            }
        })
    ).sort((a, b) => b.area - a.area);
}

export function part1(input: string) {
    const points = getPointsList(input);
    //console.log(points);
    const rectangles = getRectangles(points);
    //console.log(rectangles);
    return rectangles[0].area;
}

export function part2(input: string) {

}