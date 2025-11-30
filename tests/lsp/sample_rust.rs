// LSP Test File for Rust (rust-analyzer)
// Tests code completion, hover, and diagnostics
//
// Required: rustup component add rust-analyzer

use std::collections::HashMap;
use std::io::{self, Write};

/// Employee struct to test hover and completion
#[derive(Debug, Clone)]
struct Employee {
    name: String,
    age: u32,
    salary: f64,
}

impl Employee {
    /// Creates a new Employee
    fn new(name: String, age: u32, salary: f64) -> Self {
        Employee { name, age, salary }
    }

    /// Returns employee information as a string
    fn get_info(&self) -> String {
        format!("{}, {} years, ${:.2}", self.name, self.age, self.salary)
    }

    /// Increases salary by given percentage
    fn give_raise(&mut self, percentage: f64) {
        self.salary *= 1.0 + percentage / 100.0;
    }
}

/// Process employee data and return statistics
fn process_employees(employees: &[Employee]) -> HashMap<&str, f64> {
    let mut stats = HashMap::new();

    if employees.is_empty() {
        stats.insert("average", 0.0);
        stats.insert("total", 0.0);
        return stats;
    }

    let total: f64 = employees.iter().map(|emp| emp.salary).sum();

    // Test completion: Type 'emp.' and press Ctrl+Space
    // Should show: name, age, salary, get_info, give_raise, clone
    let emp = &employees[0];
    emp.

    stats.insert("average", total / employees.len() as f64);
    stats.insert("total", total);
    stats.insert("count", employees.len() as f64);

    stats
}

fn main() -> io::Result<()> {
    // Test hover: Position cursor on 'Employee' and press Ctrl+H
    // Should show struct documentation and fields
    let mut employees = vec![
        Employee::new("Alice".to_string(), 30, 75000.0),
        Employee::new("Bob".to_string(), 35, 85000.0),
        Employee::new("Charlie".to_string(), 28, 65000.0),
    ];

    // Test completion: Type 'Vec::' and press Ctrl+Space
    // Should show Vec associated functions
    let numbers = Vec::

    // Test completion: Type 'String::' and press Ctrl+Space
    // Should show String associated functions
    let text = String::

    let stats = process_employees(&employees);
    println!("Statistics: {:?}", stats);

    // Test hover on standard library types
    let mut output = io::stdout();
    writeln!(output, "Employee data processed")?;

    // Intentional error for diagnostics test
    // Should show: cannot find value `undefined_var` in this scope
    println!("{}", undefined_var);

    Ok(())
}