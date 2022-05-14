package main

import (
	"encoding/csv"
	"io"
	"os"
	"testing"
)

func TestRendering(t *testing.T) {

	f, err := os.Open("../testdata.csv")
	if err != nil {
		t.Error(err)
	}
	defer f.Close()
	csvReader := csv.NewReader(f)

	for {
		rec, err := csvReader.Read()
		if err == io.EOF {
			break
		}
		if err != nil {
			t.Error(err)
		}

		got := renderJson(rec[0])
		if got != rec[2] {
			t.Errorf("render(%q) == %q, want %q", rec[0], got, rec[2])
		}
	}
}

func TestHashing(t *testing.T) {

	f, err := os.Open("../testdata.csv")
	if err != nil {
		t.Error(err)
	}
	defer f.Close()
	csvReader := csv.NewReader(f)

	for {
		rec, err := csvReader.Read()
		if err == io.EOF {
			break
		}
		if err != nil {
			t.Error(err)
		}

		got := hashJson(rec[0])
		if got != rec[1] {
			t.Errorf("hash(%q) == %q, want %q", rec[0], got, rec[1])
		}
	}
}
