package day17

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"testing"
)

func expect_equal(t *testing.T, expected interface{}, actual interface{}) {
	if expected != actual {
		t.Errorf("Got %s, want %s", actual, expected)
	}
}

func get_file(file string) string {
	b, err := os.ReadFile(file)
	if err != nil {
		fmt.Print(err)
		panic("Unable to find file")
	}
	return string(b)
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

		{"adv: using combo operand B",
			`
Register A: 15
Register B: 3
Register C: 0

Program: 0,5,5,4`, "1"},

		{"adv: using combo operand C",
			`
Register A: 15
Register B: 0
Register C: 3

Program: 0,6,5,4`, "1"},

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

		{"bxc: noop xor with 0 is input",
			`
Register A: 0
Register B: 3
Register C: 0

Program: 4,0,5,5`, "3"},

		{"bxc: xor B with C",
			`
Register A: 0
Register B: 3
Register C: 1

Program: 4,0,5,5`, "2"},

		{"out: out simple value",
			`
Register A: 1
Register B: 1
Register C: 1

Program: 5,1`, "1"},

		{"out: out combo value",
			`
Register A: 1
Register B: 3
Register C: 5

Program: 5,6`, "5"},

		{"out: out modulo value",
			`
Register A: 1
Register B: 3
Register C: 12

Program: 5,6`, "4"},

		{"bdv: division of b",
			`
Register A: 9
Register B: 0
Register C: 5

Program: 6,2,5,5`, "2"},

		{"cdv: division of c",
			`
Register A: 13
Register B: 0
Register C: 0

Program: 7,2,5,6`, "3"},

		{"multiple instructions",
			`
Register A: 24
Register B: 9
Register C: 13

Program: 0,1,0,1,5,4`, "6"},

		{"multiple outputs",
			`
Register A: 24
Register B: 9
Register C: 13

Program: 0,1,0,1,5,4,5,5,5,6`, "6,1,5"},

		{"jnz: AoC sample case", get_file("sampledata.txt"), get_file("sampledata.answer.txt")},
		{"AoC test case", get_file("testdata.txt"), get_file("testdata.answer.txt")},

		{"Part 2: validate sample",
			`
Register A: 117440
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0`, "0,3,5,4,3,0"},
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

func Test_Quine(t *testing.T) {
	var tests = []struct {
		name                string
		input               string
		expected_register_A string
	}{
		{"Sample", get_file("sampledata2.txt"), get_file("sampledata.answer2.txt")},
		{"Test", get_file("testdata.txt"), get_file("testdata.answer2.txt")},
	}

	for _, tt := range tests {
		input := strings.TrimSpace(tt.input)
		testname := fmt.Sprintf("Quine Finder: %s", tt.name)
		t.Run(testname, func(t *testing.T) {

			var result = FastQuineFinder(input)

			expect_equal(t, tt.expected_register_A, strconv.FormatInt(result, 10))
		})
	}
}
