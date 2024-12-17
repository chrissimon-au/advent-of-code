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

func Test_Computer(t *testing.T) {
	var tests = []struct {
		name            string
		input           string
		expected_output string
	}{

		{"adv: Nothing to do: 0 divide by 1",
			`
Register A: 0
Register B: 0
Register C: 0

Program: 0,1,5,4`, "0"},

		{"adv: Simple Division: 4 / 2",
			`
Register A: 4
Register B: 0
Register C: 0

Program: 0,1,5,4`, "2"},

		{"adv: Rounding: 15 / 4",
			`
Register A: 15
Register B: 0
Register C: 0

Program: 0,2,5,4`, "3"},

		{"adv: using combo operand",
			`
Register A: 15
Register B: 3
Register C: 0

Program: 0,5,5,4`, "1"},

		{"bxl: Nothing to do",
			`
Register A: 15
Register B: 0
Register C: 0

Program: 1,0,5,5`, "0"},

		{"bxl: Bitwise xor with 0 returns input",
			`
Register A: 15
Register B: 3
Register C: 0

Program: 1,0,5,5`, "3"},

		{"bxl: Bitwise xor with real input",
			`
Register A: 15
Register B: 3
Register C: 0

Program: 1,1,5,5`, "2"},

		{"bst: nums < 8 return input",
			`
Register A: 0
Register B: 0
Register C: 0

Program: 2,1,5,5`, "1"},

		{"bst: nums >= 8 return modulo",
			`
Register A: 10
Register B: 0
Register C: 0

Program: 2,4,5,5`, "2"},
	}

	for _, tt := range tests {
		input := strings.TrimSpace(tt.input)
		testname := fmt.Sprintf("Test Computer: %s", tt.name)
		t.Run(testname, func(t *testing.T) {

			var result = ExecuteProgram(input)

			expect_equal(t, tt.expected_output, result)
		})
	}
}
