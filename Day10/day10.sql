DROP FUNCTION IF EXISTS create_tables;
CREATE OR REPLACE FUNCTION create_tables(
) RETURNS integer AS $$
BEGIN
    DROP TABLE IF EXISTS map_positions;
    CREATE TABLE map_positions (
        id integer,
        colIdx integer,
        rowIDx integer,
        height integer,
        paths integer[]
    );
    return 0;
END;
$$ LANGUAGE plpgsql;

DROP FUNCTION IF EXISTS insert_map;
CREATE OR REPLACE FUNCTION insert_map(
    map text
) RETURNS integer AS $$
DECLARE ch char;
DECLARE strRow varchar;
DECLARE colI integer := 0;
DECLARE rowI integer := 0;
DECLARE width integer := 0;
BEGIN
    PERFORM create_tables();    
    FOREACH strRow IN ARRAY regexp_split_to_array(map, '\n') LOOP
        width := LENGTH(strRow);
        FOREACH ch IN ARRAY regexp_split_to_array(strRow, '') LOOP            
            IF ch <> ' ' THEN
                INSERT INTO map_positions (id, colIdx, rowIdx, height) VALUES (rowI * width + colI, colI, rowI, CAST(ch AS INTEGER));
                colI := colI + 1;
            END IF;
        END LOOP;
        rowI := rowI + 1;
        colI := 0;
    END LOOP;
    return 0;
END;
$$ LANGUAGE plpgsql;

DROP FUNCTION IF EXISTS get_trailheadscore;
CREATE OR REPLACE FUNCTION get_trailheadscore(
    map text
) RETURNS integer AS $$
BEGIN
    PERFORM create_tables();    
    PERFORM insert_map(map);

    UPDATE map_positions mp SET paths =
        (SELECT array_agg(mp2.id) FROM map_positions mp2
            WHERE
                     mp2.height = mp.height - 1
                AND (
                       (mp2.colIdx=mp.colIdx + 1 AND mp2.rowIdx=mp.rowIdx)
                    OR (mp2.colIdx=mp.colIdx - 1 AND mp2.rowIdx=mp.rowIdx)
                    OR (mp2.colIdx=mp.colIdx AND mp2.rowIdx=mp.rowIdx-1)
                    OR (mp2.colIdx=mp.colIdx AND mp2.rowIdx=mp.rowIdx+1)
                )
            GROUP BY mp.id
        );

    RETURN (SELECT COUNT(1) FROM map_positions 
         WHERE height = 9 AND cardinality(paths) > 0
        );
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
    RETURN NEXT is(get_trailheadscore(
'0123456789
 5555987555'
        ), 2);
END;
$$ LANGUAGE plpgsql;

SELECT runtests( );
