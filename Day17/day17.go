package day17

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

func ExecuteProgram(input string) string {
	inputParts := strings.Split(input, "\n\n")
	registers := strings.Split(inputParts[0], "\n")
	instructions := strings.Split(inputParts[1], ",")
	registerAStr := strings.Replace(registers[0], "Register A: ", "", -1)
	registerA, _ := strconv.Atoi(registerAStr)
	divisorPower, _ := strconv.Atoi(instructions[1])
	divisor := int(math.Pow(2, float64(divisorPower)))
	result := registerA / divisor
	fmt.Printf("%s: %d %d %d %d\n", registerAStr, registerA, divisorPower, divisor, result)

	return strconv.Itoa(result)
}
