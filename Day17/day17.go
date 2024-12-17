package day17

import (
	"math"
	"strconv"
	"strings"
)

type Registers struct {
	A int
	B int
	C int
}

func ComboOperandValue(registers Registers, operand int) int {
	if operand <= 3 {
		return operand
	}
	switch operand {
	case 4:
		return registers.A
	case 5:
		return registers.B
	}
	panic("undefined")
}

func adv(registers Registers, operand int) (Registers, int) {
	opValue := ComboOperandValue(registers, operand)
	divisor := int(math.Pow(2, float64(opValue)))
	return registers, registers.A / divisor
}

func bxl(registers Registers, operand int) (Registers, int) {
	return registers, registers.B ^ operand
}

func bst(registers Registers, operand int) (Registers, int) {
	return registers, ComboOperandValue(registers, operand) % 8
}

func EvaluateOp(registers Registers, opcode int, operand int) (Registers, int) {
	switch opcode {
	case 0:
		return adv(registers, operand)
	case 1:
		return bxl(registers, operand)
	case 2:
		return bst(registers, operand)
	}
	panic("undefined opcode")
}

func ExecuteProgram(input string) string {
	inputParts := strings.Split(input, "\n\n")
	registerStrs := strings.Split(inputParts[0], "\n")
	instructions := strings.Split(strings.Replace(inputParts[1], "Program: ", "", -1), ",")
	registerAStr := strings.Replace(registerStrs[0], "Register A: ", "", -1)
	registerBStr := strings.Replace(registerStrs[1], "Register B: ", "", -1)
	registerA, _ := strconv.Atoi(registerAStr)
	registerB, _ := strconv.Atoi(registerBStr)
	opcode, _ := strconv.Atoi(instructions[0])
	operand, _ := strconv.Atoi(instructions[1])

	registers := Registers{registerA, registerB, 0}
	// fmt.Println("====")
	// fmt.Printf("%s\n", input)
	// fmt.Printf("%s: %s\n", registerAStr, instructions)
	// fmt.Printf("%d: %d %d\n", registers, opcode, operand)
	// fmt.Println("---")

	_, result := EvaluateOp(registers, opcode, operand)

	return strconv.Itoa(result)
}
