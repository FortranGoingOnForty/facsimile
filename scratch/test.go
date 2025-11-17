package main

import (
    "fmt"
    "strings"
)

// Person struct definition
type Person struct {
    Name string
    Age  int
}

// Method on Person
func (p Person) Greet() {
    fmt.Printf("Hello, I'm %s and I'm %d years old\n", p.Name, p.Age)
}

func fibonacci(n int) int {
    if n <= 1 {
        return n
    }
    return fibonacci(n-1) + fibonacci(n-2)
}

func main() {
    // Variable declarations
    var message string = "Hello, Go!"
    count := 42
    pi := 3.14159

    fmt.Println(message)

    // Create a person
    person := Person{Name: "Bob", Age: 25}
    person.Greet()

    // Slice and loop
    numbers := []int{1, 2, 3, 4, 5}
    for i, num := range numbers {
        if num%2 == 0 {
            fmt.Printf("Index %d: %d is even\n", i, num)
        }
    }

    // Channel example
    ch := make(chan string)
    go func() {
        ch <- "Goroutine message"
    }()

    msg := <-ch
    fmt.Println(msg)
}