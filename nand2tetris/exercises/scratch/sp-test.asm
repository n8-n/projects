// Testing moving the stack pointer
// this moves SP from address 5 to 10, incrementing the values
// at the address by 1 every time.


// init SP to address 5
@5
D=A
@SP
M=D

(START_INC)
@i
M=1
@LOOP
0;JMP

(START_DEC)
@i
M=-1

// we want to loop the stack pointer between address 5 and 10
(LOOP)

// inc or dec
@SP
A=M
M=M+1
@i
D=M
@SP
M=D+M

@SP
D=M
// check if we've reached 10
@10
D=D-A
@START_DEC
D;JEQ

// check if we've reached 5
@SP
D=M
@5
D=D-A
@START_INC
D;JEQ

@LOOP
0;JMP

