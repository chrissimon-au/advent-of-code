CREATE OR REPLACE FUNCTION create_tables(
) RETURNS integer AS $$
BEGIN
    DROP TABLE IF EXISTS map_positions;
    CREATE TABLE map_positions (
        colIdx integer,
        rowIDx integer,
        height integer
    );
    return 0;
END;
$$ LANGUAGE plpgsql;

DROP FUNCTION IF EXISTS get_trailheadscore;
CREATE OR REPLACE FUNCTION get_trailheadscore(
    map text
) RETURNS integer AS $$
DECLARE ch char;
BEGIN
    PERFORM create_tables();    
    FOREACH ch IN ARRAY regexp_split_to_array(map, '') LOOP
        INSERT INTO map_positions (colIdx, rowIdx, height) VALUES (0, 0, CAST(ch AS INTEGER));
    END LOOP;
    RETURN (SELECT COUNT(1) FROM map_positions WHERE height = 9);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test_day10(
) RETURNS SETOF TEXT AS $$
BEGIN        
    RETURN NEXT is(get_trailheadscore('0'), 0);
    RETURN NEXT is(get_trailheadscore('0123456789'), 1);
    RETURN NEXT is(get_trailheadscore(
'0123456789
5555595555'
        ), 1);
END;
$$ LANGUAGE plpgsql;

SELECT runtests( );