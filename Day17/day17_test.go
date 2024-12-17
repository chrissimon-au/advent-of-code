package day17

import (
	"testing"
	"fmt"
)

func expect_equal(t *testing.T, expected interface{}, actual interface{}) {
	if expected != actual {
		t.Errorf("Got %s, want %s", actual, expected)
	}
}

func Test_adv(t *testing.T) {
	var tests = []struct {
        input string
        expected_output string
    }{
        {`Register A: 0
Register B: 0
Register C: 0

Program: 0,1`, "0"},
    }

	for _, tt := range tests {
        testname := fmt.Sprintf("Test_adv: '%s','%s'", tt.input, tt.expected_output)
        t.Run(testname, func(t *testing.T) {
			
			var result = ExecuteProgram(tt.input)

			expect_equal(t, tt.expected_output, result)
        })
    }
}