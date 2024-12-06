package au.chrissimon;

import java.util.HashSet;
import java.util.Set;

enum Direction {
    Up,
    Down,
    Left,
    Right,
}


record Location(int col, int row) {
    public Location nextLocation(String mapInput, Direction direction) {
        return switch (direction) {
            case Up -> new Location(col, row - 1);
            case Down -> new Location(col, row + 1);
            case Left -> new Location(col - 1, row);
            case Right -> new Location(col + 1, row);
        };
    }
    public boolean inMap(String map) {
        return col() >= 0 && col() < map.length() && row() == 0;
    }
}
record Vector(Location location, Direction direction) {


    public Vector nextVector(String mapInput) {
        Location nextLocation = location().nextLocation(mapInput, direction());
        Direction nextDirection = direction();
        if (nextLocation.inMap(mapInput) && (mapInput.charAt(nextLocation.col()) == '#')) {
            nextDirection = Direction.values()[((direction().ordinal() + 1) % 4)];
            nextLocation = location();
        }
        return new Vector(nextLocation, nextDirection);
    }
}

public class Day06 {

    private static Vector getCurrentVector(String map) {
        int loc;
        loc = map.indexOf("^");
        if (loc >= 0) {
            return new Vector(new Location(loc, 0), Direction.Up);
        }
        loc = map.indexOf("v");
        if (loc >= 0) {
            return new Vector(new Location(loc, 0), Direction.Down);
        }
        loc = map.indexOf("<");
        if (loc >= 0) {
            return new Vector(new Location(loc, 0), Direction.Left);
        }
        loc = map.indexOf(">");
        if (loc >= 0) {
            return new Vector(new Location(loc, 0), Direction.Right);
        }
        throw new IllegalArgumentException("Invalid map: " + map);
    }

    public static int getVisitedLocationCount(String mapInput) {
        Vector currentVector = getCurrentVector(mapInput);
        Set<Location> visitedLocations = new HashSet<>();
        do {
            visitedLocations.add(currentVector.location());
            currentVector = currentVector.nextVector(mapInput);
        } while (currentVector.location().inMap(mapInput));
        return visitedLocations.size();
    }
}
