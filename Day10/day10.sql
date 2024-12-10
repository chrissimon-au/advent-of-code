CREATE OR REPLACE FUNCTION get_trailheadscore(
    map text
) RETURNS integer AS $$
BEGIN
    return 1;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test_day10(
) RETURNS SETOF TEXT AS $$
BEGIN        
    RETURN NEXT is(0, get_trailheadscore('0'));
END;
$$ LANGUAGE plpgsql;

SELECT runtests( );