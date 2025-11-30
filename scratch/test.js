// Modern JavaScript example
class Calculator {
    constructor() {
        this.result = 0;
    }

    add(a, b) {
        this.result = a + b;
        return this.result;
    }

    multiply(a, b) {
        return a * b;
    }
}

// Arrow function
const fibonacci = (n) => {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
};

// Async/await example
async function fetchData(url) {
    try {
        const response = await fetch(url);
        const data = await response.json();
        return data;
    } catch (error) {
        console.error("Error:", error);
    }
}

// Main execution
const calc = new Calculator();
console.log(`5 + 3 = ${calc.add(5, 3)}`);

// Array methods
const numbers = [1, 2, 3, 4, 5];
const doubled = numbers.map(x => x * 2);
const evens = numbers.filter(x => x % 2 === 0);

// Object destructuring
const { result } = calc;
console.log("Result:", result);

// Template literals and promises
Promise.resolve("Hello")
    .then(msg => `${msg}, World!`)
    .then(console.log);