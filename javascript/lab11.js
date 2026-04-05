// 1. Constructor
function Student(firstName, lastName, studentID) {
  this.firstName = firstName;
  this.lastName = lastName;
  this.studentID = studentID;
}

Student.prototype.display = function() {
  console.log(this.firstName, this.lastName, this.studentID);
};

// 2. Array of students
const students = [
  new Student("Kushagra", "Bainsla", 1001),
  new Student("Krishna", "Sharma", 1002),
  new Student("Nitish", "Kumar", 1003)
];

// 3. Add graduated to just one
students[0].graduated = true;

// 4. Object literal with prototype set manually
const manualStudent = {
  firstName: "Aditya",
  lastName: "Hegde",
  studentID: 1004,
  __proto__: Student.prototype
};

// Test everything
students.forEach(s => s.display());
manualStudent.display();
console.log(students[0].graduated); // true
console.log(students[1].graduated); // undefined

/*
Expected output:

Kushagra Bainsla 1001
Krishna Sharma 1002
Nitish Kumar 1003
Aditya Hegde 1004
true
undefined
*/
