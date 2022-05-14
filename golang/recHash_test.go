package main

import (
	"testing"
)

func TestReverseRunes(t *testing.T) {

	cases := []struct {
		in, want string
	}{
		{"null", "dCNOmK/nSY+12vHzasLXiswzlGT5UHA7jAGYkvmCuQs="},
		{"true", "tb6kG2xiP3wJ8b8k3K5Y66s8DN2QrZZrxDpFtEhn4Ss="},
		{"false", "/LzxZZCN0YqeSff/J4EBdtuOn2O0NSITdBZkJFIk+Ko="},
		{"123", "pmWkWSBCL51Bfkhn79xPuKBKHz//H6B+mY6G9/eieuM="},
		{"123e0", "pmWkWSBCL51Bfkhn79xPuKBKHz//H6B+mY6G9/eieuM="},
		{"123E0", "pmWkWSBCL51Bfkhn79xPuKBKHz//H6B+mY6G9/eieuM="},
		{"123e4", "GJVCYtsVbXtVe5V+GzGh8SrKOcCew4YlJ0DXtOHBlEY="},
		{"123E4", "GJVCYtsVbXtVe5V+GzGh8SrKOcCew4YlJ0DXtOHBlEY="},
		{"\"example\"", "LiMlAWSVfVGRFfLdJTOE5IeAbofLNVfrE3K5N9/5OVk="},
		{"[]", "T1PNoYwrqgwDVLtfmj7L5e0Sq02OEbqHPC8RFhICuUU="},
		{"[123, \"456\"]", "oVOjk6OsPCtrkMnCxtLVc5Wz8GFRUg/hhdMMEvnmCOg="},
		{"[\"456\", 123]", "V/ajXCUo/6He2Rh/lh7P3rAwuSexhas8AaZ9mLJFKi8="},
		{"{}", "RBNvo1WzZ4oRRq0W9+hknpT7T8If536DEMBg9hyq/4o="},
	}
	for _, c := range cases {
		got := recHashJson(c.in)
		if got != c.want {
			t.Errorf("recHashJson(%q) == %q, want %q", c.in, got, c.want)
		}
	}
}
