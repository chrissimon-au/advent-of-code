package day17

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

type Registers struct {
	A  int64
	B  int64
	C  int64
	ip int
}

type Operation struct {
	opcode  int
	operand int
}

func ComboOperandValue(registers Registers, operand int) int64 {
	if operand <= 3 {
		return int64(operand)
	}
	switch operand {
	case 4:
		return registers.A
	case 5:
		return registers.B
	case 6:
		return registers.C
	}
	panic("undefined")
}

func divide(registers Registers, numerator int64, operand int) int64 {
	opValue := ComboOperandValue(registers, operand)
	divisor := int64(math.Pow(2, float64(opValue)))
	return (numerator / divisor)
}

func adv(registers Registers, operand int) Registers {
	result := divide(registers, registers.A, operand)
	registers.A = result
	return move_ip(registers)
}

func bxl(registers Registers, operand int) Registers {
	registers.B = registers.B ^ int64(operand)
	return move_ip(registers)
}

func bst(registers Registers, operand int) Registers {
	registers.B = ComboOperandValue(registers, operand) % 8
	return move_ip(registers)
}

func bxc(registers Registers) Registers {
	registers.B = registers.B ^ registers.C
	return move_ip(registers)
}

func out(registers Registers, operand int) (Registers, string) {
	return move_ip(registers), strconv.FormatInt(ComboOperandValue(registers, operand)%8, 10)
}

func bdv(registers Registers, operand int) Registers {
	registers.B = divide(registers, registers.A, operand)
	return move_ip(registers)
}

func cdv(registers Registers, operand int) Registers {
	registers.C = divide(registers, registers.A, operand)
	return move_ip(registers)
}

func jnz(registers Registers, operand int) Registers {
	if registers.A == 0 {
		return move_ip(registers)
	}
	registers.ip = operand
	return registers
}

func move_ip(registers Registers) Registers {
	registers.ip++
	return registers
}

func EvaluateOp(registers Registers, op Operation) (Registers, string) {
	switch op.opcode {
	case 0:
		return adv(registers, op.operand), ""
	case 1:
		return bxl(registers, op.operand), ""
	case 2:
		return bst(registers, op.operand), ""
	case 3:
		return jnz(registers, op.operand), ""
	case 4:
		return bxc(registers), ""
	case 5:
		return out(registers, op.operand)
	case 6:
		return bdv(registers, op.operand), ""
	case 7:
		return cdv(registers, op.operand), ""
	}
	panic("undefined opcode")
}

func ParseRegisters(registerStr string) Registers {
	registerStrs := strings.Split(registerStr, "\n")
	registerAStr := strings.Replace(registerStrs[0], "Register A: ", "", -1)
	registerBStr := strings.Replace(registerStrs[1], "Register B: ", "", -1)
	registerCStr := strings.Replace(registerStrs[2], "Register C: ", "", -1)
	registerA, _ := strconv.ParseInt(registerAStr, 10, 64)
	registerB, _ := strconv.ParseInt(registerBStr, 10, 64)
	registerC, _ := strconv.ParseInt(registerCStr, 10, 64)

	return Registers{registerA, registerB, registerC, 0}
}

func ParseProgram(program string) []Operation {
	operationsRaw := strings.Split(strings.Replace(program, "Program: ", "", -1), ",")
	numOps := len(operationsRaw) / 2
	operations := make([]Operation, numOps)
	for i := 0; i < numOps; i++ {
		opcode, _ := strconv.Atoi(operationsRaw[i*2])
		operand, _ := strconv.Atoi(operationsRaw[i*2+1])
		op := Operation{opcode, operand}
		operations[i] = op
	}
	return operations
}

const LOG bool = false

func logState(input string, registers Registers, operations []Operation) {
	fmt.Println("====")
	fmt.Printf("%s\n", input)
	fmt.Println("-")
	fmt.Printf("Registers: %d\n", registers)
	fmt.Printf("op: %d\n", operations)
	fmt.Println("----")
}

func ParseInput(input string) (Registers, []Operation, string) {
	inputParts := strings.Split(input, "\n\n")
	registers := ParseRegisters(inputParts[0])

	operations := ParseProgram(inputParts[1])

	if LOG {
		logState(input, registers, operations)
	}
	return registers, operations, inputParts[1]
}

func ExecuteOperations(registers Registers, operations []Operation) string {
	var outputs []string
	var output string
	for registers.ip < len(operations) {
		op := operations[registers.ip]
		registers, output = EvaluateOp(registers, op)
		if len(output) > 0 {
			outputs = append(outputs, output)
		}
	}

	full_output := strings.Join(outputs, ",")

	if LOG {
		fmt.Printf("Output: %s\n", full_output)
	}

	return full_output
}

func ExecuteProgram(input string) string {
	registers, operations, _ := ParseInput(input)
	return ExecuteOperations(registers, operations)
}

func FindQuine(input string) int64 {
	registers, operations, program_listing := ParseInput(input)
	output := ExecuteOperations(registers, operations)

	fmt.Printf("Output: %s, Wanted: %s", output, program_listing)
	return registers.A
}
