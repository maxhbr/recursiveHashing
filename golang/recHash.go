package main

import (
	"crypto/sha256"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"sort"
)

func hashBytes(input []byte) string {
	hasher := sha256.New()
	hasher.Write(input)
	return base64.StdEncoding.EncodeToString(hasher.Sum(nil))
}

func hashString(input string) string {
	return hashBytes([]byte(input))
}

func render(input interface{}) string {
	switch v := input.(type) {
	default:
		err := fmt.Errorf("unexpected type %T", v)
		fmt.Println(err)
		return "err."
	case nil:
		return "null"
	case []interface{}:
		acc := "["
		for i, element := range input.([]interface{}) {
			if i != 0 {
				acc += ","
			}
			acc += hash(element)
		}
		acc += "]"
		return acc
	case map[string]interface{}:
		var hashes []string
		for key, element := range input.(map[string]interface{}) {
			hashes = append(hashes, hashString(key+":"+hash(element)))
		}
		sort.Strings(hashes)
		acc := "{"
		for i, element := range hashes {
			if i != 0 {
				acc += ","
			}
			acc += element
		}
		acc += "}"
		return acc
	case bool:
		if input.(bool) {
			return "true"
		} else {
			return "false"
		}
	case float64:
		return fmt.Sprint(int(input.(float64)))
	case string:
		return "\"" + input.(string) + "\""
	}
}

func hash(input interface{}) string {
	rendered := render(input)
	hashed := hashString(rendered)
	fmt.Printf("%s -> %s\n", rendered, hashed)
	return hashed
}

func parseJson(input string) interface{} {
	var m interface{}
	err := json.Unmarshal([]byte(input), &m)
	if err == nil {
		return m
	}
	return nil
}

func renderJson(input string) string {
	return render(parseJson(input))
}

func hashJson(input string) string {
	return hash(parseJson(input))
}
