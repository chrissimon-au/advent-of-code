CREATE OR REPLACE FUNCTION get_trailheadscore(
    map text
) RETURNS integer AS $$
BEGIN
    return 0;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test_day10(
) RETURNS SETOF TEXT AS $$
BEGIN        
    RETURN NEXT is(get_trailheadscore('0'), 0);
    RETURN NEXT is(get_trailheadscore('0123456789'), 1);
END;
$$ LANGUAGE plpgsql;

SELECT runtests( );