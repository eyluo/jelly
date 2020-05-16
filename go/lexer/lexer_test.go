package lexer

import (
	"io/ioutil"
	"testing"
)

func TestLegalLexers(t *testing.T) {
	t.Log("Testing lexer on correct programs...")
	testf := "../../tests/add.test"
	lxr, err := NewLexer(testf)
	if err != nil {
		t.Fatalf("Lexer encountered an unexpected error for %s\n", testf)
	}

	count := 1
	temp := lxr.Pop()
	if temp.Type != IntVal || temp.Val != 1 {
		t.Fatalf("%s: incorrectly parsed token %d. Expected %s, got %s\n",
			testf,
			count,
			(&Token{Type: IntVal, Val: 1}).ToString(),
			temp.ToString(),
		)
	}
	count++
	temp = lxr.Pop()
	if temp.Type != Operator || temp.Optr != Plus {
		t.Fatalf("%s: incorrectly parsed token %d. Expected %s, got %s\n",
			testf,
			count,
			(&Token{Type: Operator, Optr: Plus}).ToString(),
			temp.ToString(),
		)
	}
	count++
	temp = lxr.Pop()
	if temp.Type != IntVal || temp.Val != 1 {
		t.Fatalf("%s: incorrectly parsed token %d. Expected %s, got %s\n",
			testf,
			count,
			(&Token{Type: IntVal, Val: 1}).ToString(),
			temp.ToString(),
		)
	}

	testPath := "../../tests"
	fileInfo, err := ioutil.ReadDir(testPath)
	if err != nil {
		t.Fatal(err)
	}
	for _, f := range fileInfo {
		if f.IsDir() {
			continue
		}
		lxr, err := NewLexer(testPath + "/" + f.Name())
		if err != nil {
			t.Fatal(f.Name(), "- could not create lexer for legal file", err)
		}
		tokenStr := ""
		for temp := lxr.Pop(); temp.Type != Eof; temp = lxr.Pop() {
			tokenStr += temp.ToString()
		}
		t.Log(tokenStr)
	}
}

func TestIllegalLexers(t *testing.T) {
	t.Log("Testing lexer catches illegal behavior...")
	testf := "../../tests/bad/illegalchar.test"
	_, err := NewLexer(testf)
	if err == nil {
		t.Fatalf("Lexer should have caught illegal character\n")
	} else {
		t.Logf("%s\n", err.Error())
	}

	testStr := "1direction = 1+1+1+1+1;"
	_, err = newLexer(testStr)
	if err == nil {
		t.Fatalf("Lexer should have caught illegal symbol\n")
	} else {
		t.Logf("%s\n", err.Error())
	}
}
