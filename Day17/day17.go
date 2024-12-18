package day17

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

type Registers struct {
	A int
	B int
	C int
}

type Operation struct {
	opcode  int
	operand int
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
	case 6:
		return registers.C
	}
	panic("undefined")
}

func divide(registers Registers, numerator int, operand int) int {
	opValue := ComboOperandValue(registers, operand)
	divisor := int(math.Pow(2, float64(opValue)))
	return (numerator / divisor)
}

func adv(registers Registers, operand int) Registers {
	result := divide(registers, registers.A, operand)
	registers.A = result
	return registers
}

func bxl(registers Registers, operand int) Registers {
	registers.B = registers.B ^ operand
	return registers
}

func bst(registers Registers, operand int) Registers {
	registers.B = ComboOperandValue(registers, operand) % 8
	return registers
}

func bxc(registers Registers) Registers {
	registers.B = registers.B ^ registers.C
	return registers
}

func out(registers Registers, operand int) (Registers, string) {
	return registers, strconv.Itoa(ComboOperandValue(registers, operand) % 8)
}

func bdv(registers Registers, operand int) Registers {
	registers.B = divide(registers, registers.A, operand)
	return registers
}

func cdv(registers Registers, operand int) Registers {
	registers.C = divide(registers, registers.A, operand)
	return registers
}

func jnz(registers Registers, operand int) (Registers, int, bool) {
	if registers.A == 0 {
		return registers, 0, false
	}
	return registers, operand, true
}

func EvaluateOp(registers Registers, op Operation, instruction_pointer int) (Registers, string, int) {
	new_instruction_pointer := instruction_pointer + 1
	switch op.opcode {
	case 0:
		return adv(registers, op.operand), "", new_instruction_pointer
	case 1:
		return bxl(registers, op.operand), "", new_instruction_pointer
	case 2:
		return bst(registers, op.operand), "", new_instruction_pointer
	case 3:
		r, ip, jumped := jnz(registers, op.operand)
		if jumped {
			return r, "", ip
		} else {
			return r, "", new_instruction_pointer
		}
	case 4:
		return bxc(registers), "", new_instruction_pointer
	case 5:
		r, output := out(registers, op.operand)
		return r, output, new_instruction_pointer
	case 6:
		return bdv(registers, op.operand), "", new_instruction_pointer
	case 7:
		return cdv(registers, op.operand), "", new_instruction_pointer
	}
	panic("undefined opcode")
}

func ParseRegisters(registerStr string) Registers {
	registerStrs := strings.Split(registerStr, "\n")
	registerAStr := strings.Replace(registerStrs[0], "Register A: ", "", -1)
	registerBStr := strings.Replace(registerStrs[1], "Register B: ", "", -1)
	registerCStr := strings.Replace(registerStrs[2], "Register C: ", "", -1)
	registerA, _ := strconv.Atoi(registerAStr)
	registerB, _ := strconv.Atoi(registerBStr)
	registerC, _ := strconv.Atoi(registerCStr)

	return Registers{registerA, registerB, registerC}
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

func logState(input string, registers Registers, operations []Operation) {
	fmt.Println("====")
	fmt.Printf("%s\n", input)
	fmt.Println("-")
	fmt.Printf("Registers: %d\n", registers)
	fmt.Printf("op: %d\n", operations)
	fmt.Println("----")
}

func ExecuteProgram(input string) string {
	inputParts := strings.Split(input, "\n\n")
	registers := ParseRegisters(inputParts[0])

	operations := ParseProgram(inputParts[1])

	if true {
		logState(input, registers, operations)
	}

	var outputs []string
	var output string
	instruction_pointer := 0
	for instruction_pointer < len(operations) {
		op := operations[instruction_pointer]
		registers, output, instruction_pointer = EvaluateOp(registers, op, instruction_pointer)
		if len(output) > 0 {
			outputs = append(outputs, output)
		}
	}

	return strings.Join(outputs, ",")
}
