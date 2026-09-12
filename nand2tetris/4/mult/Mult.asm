// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)
// The algorithm is based on repetitive addition.

(INIT)
    @R2
    M=0

(LOOP)
    // check if R1 is 0
    @R1
    D=M
    @END
    D;JEQ
    // decrement R1
    @R1
    M=M-1
    // get R0
    @R0
    D=M
    // get R3 and add
    @R2
    M=D+M
    // Loop
    @LOOP
    0;JMP

(END)
    @END
    0;JMP
