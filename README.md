# Matrix-and-Vector-Manipulation

This code aims to do vector and matrix manipulation. This code is written in OCaml.

## Vectors

Vectors are defined to be a list of floats.

### List of functions :

*Note: "n" used in the time complexities is the length of the vector

1. Vdim: This return the length of the vector. *Time Complexity : O(n)*
2. mkzerov: This make a zero vector of the given length, a zero vector is a vector with all values equal to zero. *Time Complexity: O(n)*
3. iszerov: This checks whether the vector is a zero vector or not. *Time Complexity : O(n)*
4. addv: This adds the two vectors, if they have different length then a exception is thrown. *Time Complexity : O(n)*
5. scalarmultv: This multiplies a scalar to the vector. *Time Complexity : O(n)*
6. crossprodv: Multiplies two vectors with each other. 

## Matrix

A matrix are defined to be a list of list of floats.

### List of functions :

*Note: "n" used in the time complexities is the rows and m is the columns

1. mdim: This return the dimensions of the matrix of the vector. *Time Complexity : O(m*n)*
2. mkzerom: This make a zero vector of the given dimensions, a zero matrix is a matrix with all values equal to zero. *Time Complexity: O(m*n)*
3. iszerom: This checks whether the Matrix is a zero vector or not. *Time Complexity : O(m*n)*
4. addm: This adds the two matrices, if they have different dimensions then a exception is thrown. *Time Complexity : O(m*n)*
5. scalarmultm: This multiplies a scalar to the matrix. *Time Complexity : O(m*n)*
6. transm: Returns the transpose of the matrix
7. detm: Calculates the determinant of the matrix
8. multm: Multiplies two matrices
9. invm: Finds the inverse of the matrix
