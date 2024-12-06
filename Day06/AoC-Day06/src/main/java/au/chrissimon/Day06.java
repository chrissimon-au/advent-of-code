package au.chrissimon;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

enum Direction {
    Up,
    Right,
    Down,
    Left;

    public static Direction fromChar(char c) {
        return switch (c) {
            case '^' -> Direction.Up;
            case 'v' -> Direction.Down;
            case '<' -> Direction.Left;
            case '>' -> Direction.Right;
            default -> throw new IllegalArgumentException("Character is not a direction: " + c);
        };
    }

    public Direction next() {
        return Direction.values()[((this.ordinal() + 1) % 4)];
    }
}

record RouteResult(List<Vector> routePositions, boolean loops) {}

class Map {
    public Map(String mapInput) {
        _mapRows = mapInput.split("\n");
    }

    private final String[] _mapRows;

    public boolean isBlockerAt(Location location) {
        if (!inMap(location)) {
            return false;
        }
        return _mapRows[location.row()].charAt(location.col()) == '#';
    }
    public boolean inMap(Location location) {
        return  location.col() >= 0 &&
                location.col() < _mapRows[0].length() &&
                location.row() >= 0 &&
                location.row() < _mapRows.length;
    }

    private static final String GUARD_POSITION = ".*[\\^v<>].*";
    public Vector getCurrentVector() {
        int row = IntStream.range(0, _mapRows.length).filter(i -> _mapRows[i].matches(GUARD_POSITION)).findFirst().orElse(-1);
        String mapRow = _mapRows[row];
        int col = IntStream.range(0, mapRow.length()).filter(i -> String.valueOf(mapRow.charAt(i)).matches(GUARD_POSITION)).findFirst().orElse(-1);
        Location location = new Location(col, row);
        Direction direction = Direction.fromChar(mapRow.charAt(location.col()));
        return new Vector(location, direction);
    }

    public RouteResult executeRoute() {
        Vector currentVector = getCurrentVector();
        List<Vector> routePositions = new ArrayList<>();
        do {
            routePositions.add(currentVector);
            currentVector = currentVector.nextVector(this);
            if (routePositions.contains(currentVector)) {
                return new RouteResult(routePositions, true);
            }
        } while (inMap(currentVector.location()));
        return new RouteResult(routePositions, false);
    }

    public void createBlock(Location location) {
        String row = _mapRows[location.row()];
        StringBuilder newRow = new StringBuilder(row);
        newRow.setCharAt(location.col(), '#');
        _mapRows[location.row()] = newRow.toString();
    }
}


record Location(int col, int row) {
    public Location nextLocation(Direction direction) {
        return switch (direction) {
            case Up -> new Location(col, row - 1);
            case Down -> new Location(col, row + 1);
            case Left -> new Location(col - 1, row);
            case Right -> new Location(col + 1, row);
        };
    }

}
record Vector(Location location, Direction direction) {

    public Vector nextVector(Map map) {
        Location nextLocation = location().nextLocation(direction());
        Direction nextDirection = direction();
        if (map.isBlockerAt(nextLocation)) {
            nextDirection = direction.next();
            nextLocation = location();
        }
        return new Vector(nextLocation, nextDirection);
    }
}

public class Day06 {

    public static int getVisitedLocationCount(String mapInput) {
        Map map = new Map(mapInput);
        RouteResult result = map.executeRoute();
        List<Vector> routePositions = result.routePositions();
        java.util.Map<Location, List<Vector>> locations =
                routePositions.stream().collect(Collectors.groupingBy(Vector::location));
        return locations.keySet().size();
    }

    public static int getNumberOfLoopInterventionPoints(String mapInput) {
        Map map = new Map(mapInput);
        RouteResult result = map.executeRoute();
        Set<Location> loopLocations = new HashSet<>();
        Vector startVector = map.getCurrentVector();
        java.util.Map<Location, List<Vector>> locations =
                result.routePositions().stream().collect(Collectors.groupingBy(Vector::location));
        for (Location routeLocation : locations.keySet()) {
            if (!routeLocation.equals(startVector.location())) {
                Map newMap = new Map(mapInput);
                newMap.createBlock(routeLocation);
                RouteResult newResult = newMap.executeRoute();
                if (newResult.loops()) {
                    loopLocations.add(routeLocation);
                }
            }
        }
        return loopLocations.size();
    }
}
