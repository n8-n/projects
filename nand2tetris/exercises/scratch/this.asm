// this and that test with stack pointer

// stack pointer at 10
@10
D=A
@SP
M=D

// this at 20
// 22: value to 111
@20
D=A
@THIS
M=D

@111
D=A

@THIS
A=M+1
M=D


// push this 1
@THIS
D=M
@1
A=D+A
D=M

@SP
A=M
M=D // then decrement


// what to do here? how to hold the address for (this 2) and the popped value?
// pop this 2
@THIS
D=M
@2
A=D+A
D=A

@SP
A=M
D=M // then increment SP






