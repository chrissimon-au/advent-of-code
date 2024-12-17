package day17

import (
	"fmt"
	"strings"
	"testing"
)

func expect_equal(t *testing.T, expected interface{}, actual interface{}) {
	if expected != actual {
		t.Errorf("Got %s, want %s", actual, expected)
	}
}

func Test_adv(t *testing.T) {
	var tests = []struct {
		input           string
		expected_output string
	}{
		{`
Register A: 0
Register B: 0
Register C: 0

Program: 0,1,5,4`, "0"},

		{`
Register A: 4
Register B: 0
Register C: 0

Program: 0,1,5,4`, "2"},

		{`
Register A: 15
Register B: 0
Register C: 0

Program: 0,2,5,4`, "3"},
	}

	for _, tt := range tests {
		input := strings.TrimSpace(tt.input)
		testname := fmt.Sprintf("Test_adv: '%s','%s'", input, tt.expected_output)
		t.Run(testname, func(t *testing.T) {

			var result = ExecuteProgram(input)

			expect_equal(t, tt.expected_output, result)
		})
	}
}
