// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.

// Runs an infinite loop that listens to the keyboard input. 
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, 
// the screen should be cleared.


// get screen address and store in R0, 
// which will be used as an address counter
@SCREEN
D=A
@screenAddress
M=D

// final screen address constant
@8192
D=A
@finalAddress
M=D // init finalAddress to 8192

@screenAddress
D=M
@finalAddress
M=D+M   // set finalAddress to end of screenMemory (16384 + 8192)

(LOOP)
    // End if we've reached final address
    @screenAddress
    D=M
    @finalAddress
    D=D-M
    @END
    D;JEQ

    // paint it black
    @screenAddress
    A=M
    M=1 
    M=-M    // -1 = 16 bit number with all 1s

    @screenAddress
    M=M+1

    @LOOP
    0;JMP

(END)
    @END
    0;JMP
