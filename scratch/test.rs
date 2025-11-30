use std::collections::HashMap;

// A simple struct
#[derive(Debug)]
struct Person {
    name: String,
    age: u32,
}

impl Person {
    fn new(name: &str, age: u32) -> Self {
        Person {
            name: name.to_string(),
            age,
        }
    }

    fn greet(&self) {
        println!("Hello, I'm {} and I'm {} years old", self.name, self.age);
    }
}

fn main() {
    // Create a person
    let person = Person::new("Alice", 30);
    person.greet();

    // Use a HashMap
    let mut scores = HashMap::new();
    scores.insert("Blue", 10);
    scores.insert("Red", 50);

    // Pattern matching
    match scores.get("Blue") {
        Some(score) => println!("Blue team score: {}", score),
        None => println!("No score found"),
    }

    // Iterate with closure
    let numbers = vec![1, 2, 3, 4, 5];
    let doubled: Vec<i32> = numbers.iter().map(|x| x * 2).collect();
    println!("Doubled: {:?}", doubled);
}