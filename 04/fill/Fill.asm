// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

    // n = 8192
    @8192
    D=A
    @n
    M=D

(KEYBOARD)
    // i = 0
    @i
    M=0

    // addr = 16384
    @SCREEN
    D=A
    @addr
    M=D

    // if 0 GOTO WHITE
    @KBD
    D=M
    @WHITE
    D;JEQ

(BLACK)
    // GOTO KEYBOARD if i = n
    @i
    D=M
    @n
    D=D-M
    @KEYBOARD
    D;JEQ

    // set screen row to -1 (BLACK)
    @addr
    A=M
    M=-1

    // increment addr by 1
    @addr
    M=M+1

    // increment i by 1
    @i
    M=M+1

    // GOTO BLACK
    @BLACK
    0;JMP

(WHITE)
    // GOTO KEYBOARD if i = n
    @i
    D=M
    @n
    D=D-M
    @KEYBOARD
    D;JEQ

    // set screen row to 0 (WHITE)
    @addr
    A=M
    M=0

    // increment addr by 1
    @addr
    M=M+1

    // increment i by 1
    @i
    M=M+1

    // GOTO WHITE
    @WHITE
    0;JMP

(END)
    @END
    0;JMP
