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

func hashJsonNull() string {
	return hashBytes([]byte("null"))
}

func hashJsonBool(input bool) string {
	if input {
		return hashBytes([]byte("true"))
	} else {
		return hashBytes([]byte("false"))
	}
}

func hashJsonNumber(input float64) string {
	return hashBytes([]byte(fmt.Sprint(int(input))))
}

func hashJsonString(input string) string {
	return hashBytes([]byte("\"" + input + "\""))
}

func recHash(input interface{}) string {
	switch v := input.(type) {
	default:
		err := fmt.Errorf("unexpected type %T", v)
		fmt.Println(err)
		return "err."
	case nil:
		return hashJsonNull()
	case []interface{}:
		acc := "["
		for i, element := range input.([]interface{}) {
			if i != 0 {
				acc += ","
			}
			acc += recHash(element)
		}
		acc += "]"
		return hashBytes([]byte(acc))
	case map[string]interface{}:
		var hashes []string
		for key, element := range input.(map[string]interface{}) {
			hashes = append(hashes, hashBytes([]byte(key+":"+recHash(element))))
		}
		sort.Strings(hashes)
		acc := "{"
		for i, element := range hashes {
			if i != 0 {
				acc += ","
			}
			acc += recHash(element)
		}
		acc += "}"
		return hashBytes([]byte(acc))
	case bool:
		return hashJsonBool(input.(bool))
	case float64:
		return hashJsonNumber(input.(float64))
	case string:
		return hashJsonString(input.(string))
	}
}

func parseJson(input string) interface{} {
	var m interface{}
	err := json.Unmarshal([]byte(input), &m)
	if err == nil {
		return m
	}
	return nil
}

func recHashJson(input string) string {
	return recHash(parseJson(input))
}

func main() {
	fmt.Println("Hello, world.")
}
