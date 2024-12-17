package main

import (
	"testing"
)

func TestCheck(t *testing.T) {
	ans := Check()
	if !ans {
		t.Errorf("got %t, want %t", ans, true)
	}
}