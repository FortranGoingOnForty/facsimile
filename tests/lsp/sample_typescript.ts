// LSP Test File for TypeScript (typescript-language-server)
// Tests code completion, hover, and diagnostics
//
// Required: npm install -g typescript typescript-language-server

interface Employee {
    name: string;
    age: number;
    salary: number;
}

class EmployeeManager {
    private employees: Employee[] = [];

    /**
     * Add a new employee to the manager
     * @param employee The employee to add
     */
    addEmployee(employee: Employee): void {
        this.employees.push(employee);
    }

    /**
     * Get employee by name
     * @param name The name to search for
     * @returns The employee or undefined if not found
     */
    getEmployee(name: string): Employee | undefined {
        return this.employees.find(emp => emp.name === name);
    }

    /**
     * Calculate average salary
     * @returns The average salary of all employees
     */
    getAverageSalary(): number {
        if (this.employees.length === 0) return 0;

        const total = this.employees.reduce((sum, emp) => sum + emp.salary, 0);
        return total / this.employees.length;
    }

    /**
     * Give raise to all employees
     * @param percentage The percentage increase
     */
    giveRaiseToAll(percentage: number): void {
        this.employees.forEach(emp => {
            emp.salary *= (1 + percentage / 100);
        });
    }
}

// Test function with generic types
function processData<T extends Employee>(data: T[]): Map<string, number> {
    const result = new Map<string, number>();

    // Test completion: Type 'data[0].' and press Ctrl+Space
    // Should show: name, age, salary
    if (data.length > 0) {
        const first = data[0];
        first.
    }

    // Test completion: Type 'result.' and press Ctrl+Space
    // Should show Map methods: set, get, has, delete, clear, etc.
    result.

    data.forEach(item => {
        result.set(item.name, item.salary);
    });

    return result;
}

// Main execution
function main(): void {
    // Test hover: Position cursor on 'EmployeeManager' and press Ctrl+H
    // Should show class documentation
    const manager = new EmployeeManager();

    // Test completion: Type 'manager.' and press Ctrl+Space
    // Should show: addEmployee, getEmployee, getAverageSalary, giveRaiseToAll
    manager.

    const employees: Employee[] = [
        { name: "Alice", age: 30, salary: 75000 },
        { name: "Bob", age: 35, salary: 85000 },
        { name: "Charlie", age: 28, salary: 65000 }
    ];

    // Test completion: Type 'Array.' and press Ctrl+Space
    // Should show Array static methods
    const numbers = Array.

    // Test completion: Type 'console.' and press Ctrl+Space
    // Should show: log, error, warn, info, debug, etc.
    console.

    employees.forEach(emp => manager.addEmployee(emp));

    const avgSalary = manager.getAverageSalary();
    console.log(`Average salary: $${avgSalary.toFixed(2)}`);

    // Intentional error for diagnostics test
    // Should show: Cannot find name 'undefinedVariable'
    console.log(undefinedVariable);

    // Type error for diagnostics test
    // Should show: Type 'string' is not assignable to type 'number'
    const count: number = "not a number";
}

main();