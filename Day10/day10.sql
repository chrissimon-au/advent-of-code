CREATE OR REPLACE FUNCTION test_day10(
) RETURNS SETOF TEXT AS $$
BEGIN
    RETURN NEXT pass( 'plpgsql simple' );
    RETURN NEXT pass( 'plpgsql simple 2' );
    RETURN NEXT pass( 'plpgsql simple 2' );
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test_day10_p2(
) RETURNS SETOF TEXT AS $$
BEGIN
    RETURN NEXT pass( 'part2' );
    RETURN NEXT pass( '??' );
END;
$$ LANGUAGE plpgsql;

SELECT runtests( );