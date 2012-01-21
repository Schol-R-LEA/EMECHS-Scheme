# EMECHS Scheme (C) Copyright Joseph Osako Jr 2011
#
# This software is freely distributable under the terms of the GNU
# General Public License version 3. See the file LICENSE for details.
################################################################################
#stack offsets
fp.ra = 4
fp.a0 = 8
fp.a1 = 12
fp.a2 = 16
fp.a3 = 20
fp.s0 = -4
fp.s1 = -8
fp.s2 = -12
fp.s3 = -16
fp.s4 = -20
fp.s5 = -24
fp.s6 = -28
fp.s7 = -32
################################################################################
#System calls
print_int   = 01 #$a0  = integer
print_float = 02 #$f12 = float
print_double= 03 #$f12 = double
print_string= 04 #$a0  = string
read_int    = 05 #returns integer in $v0
read_float  = 06 #returns float   in $f0
read_double = 07 #returns double  in $f0
read_string = 08 #$a0 = buffer, $a1 = length
malloc      = 09 #$a0 = size of request returns pointer in $v0
Exit        = 10
print_char  = 11 #$a0 = char
read_char   = 12 #returns char (in $v0)
open        = 13 #$a0 = filename (string), $a1 =flags, $a2 = mode 0=read 1= wr 2=rw 8=append
                 #         returns file descriptor (in $v0)
read        = 14 #$a0 = file descriptor, $a1 = buffer, $a2 = length
                 #         returns num chars read (in $v0 )
write       = 15 #$a0 = file descriptor, $a1 = buffer,$a2 = length
                 #         returns num chars written (in $v0 )
close       = 16 #$a0 = file descriptor
exit2       = 17 #$a0 = result
################################################################################

# object types
TYPE.EMPTY_LIST = 0x00
TYPE.PAIR       = 0x01
TYPE.INT        = 0x02
TYPE.FIXNUM     = 0x03
TYPE.FLONUM     = 0x04
TYPE.RATIONAL   = 0x05
TYPE.COMPLEX    = 0x06
TYPE.CHAR       = 0x07
TYPE.STRING     = 0x08
TYPE.SYMBOL     = 0x09
TYPE.BOOLEAN    = 0x0a
TYPE.VECTOR     = 0x0b
TYPE.LAMBDA     = 0x0c
TYPE.PRIMITIVE  = 0x0d
TYPE.SPECIAL    = 0x0e    # special characters such as periods and quotes
TYPE.FORM       = 0x0f
TYPE.COMMENT    = 0x10
TYPE.ERROR      = 0xffffffff

# offsets defining the position of different possible values
# within an object
object             = 0
object.sizeof      = 12
object.type        = 0
object.car         = 4
object.cdr         = 8
object.integer     = 4    # 32-bit integer
object.intstring   = 8    # pointer to a string object representing the integer
object.float       = 4    # 64-bit floating-point number
object.fixednum    = 4    # ptr to an integer representing the integral part
object.fixeddec    = 8    # ptr to an integer representing the fractional part
object.numerator   = 4    # ptr to an integer
object.denominator = 8    # ptr to an integer
object.real        = 4    # ptr to a float - real part of a complex number
object.imaginary   = 8    # ptr to a float - imaginary part of a complex number
object.char        = 4
object.strptr      = 4
object.strlen      = 8 
object.symname     = 4    # name of the symbol
object.symval      = 8    # ptr to the symbol's value
object.boolval     = 4
object.vecval      = 4
object.veclen      = 8
object.lambda      = 4    # ptr to the function's list representation
object.env         = 8    # ptr to the function's environment
object.formname    = 4    # pointer to the name of a form
object.form        = 8    # pointer to a built-in form
object.funname     = 4    # pointer to the name of a form
object.function    = 8    # pointer to a built-in form
object.special     = 4
object.errcode     = 4

print_jumps.entry_size = 8
form_jumps.entry_size = 8
eval_jumps.entry_size = 8

# error types
err.unhandled_error       = 0x00
err.unknown_object_type   = 0x01


################################################################################
# Global data structures
###############################
.data

heap:          .space  0x10000
swap:          .space  0x10000

input_buffer:  .space  0x1000

TOP_ENV:       .space  0x1000
heap_pointer:  .space  4
swap_pointer:  .space  4
escape_frame:  .space  4             # holds initial stack pointer to allow the program 
                                     # to escape to the top-level in case of errors
.align 4


###############################
# Object Singletons
###############################
null:          .word TYPE.EMPTY_LIST, 0x00, 0x00
true:          .word TYPE.BOOLEAN, 0x01, 0x00
false:         .word TYPE.BOOLEAN, 0x00, 0x00
period:        .word TYPE.SPECIAL, 0x2e, 0x00
quote:         .word TYPE.SPECIAL, 0x27, 0x00
quasiquote:    .word TYPE.SPECIAL, 0x2c, 0x00
at:            .word TYPE.SPECIAL, 0x40, 0x00


###############################
# Special Forms Table
###############################
special_forms:
n_eval:       .asciiz "eval"
n_apply:      .asciiz "apply"
n_define:     .asciiz "define"
n_set:        .asciiz "set!"
n_set_car:    .asciiz "set-car!"
n_set_cdr:    .asciiz "set-cdr!"
n_cons:       .asciiz "cons"
n_car:        .asciiz "car"
n_cdr:        .asciiz "cdr"
n_lambda:     .asciiz "lambda"
n_let:        .asciiz "let"
n_let_star:   .asciiz "let*"
n_letrec:     .asciiz "letrec"
n_begin:      .asciiz "begin"
n_if:         .asciiz "if"
n_cond:       .asciiz "cond"
n_case:       .asciiz "case"
n_quote:      .asciiz "quote"
n_quasiquote: .asciiz "quasiquote"

special_end:  .byte   0

primitive_forms:
pn_plus:       .asciiz "plus"
ps_plus:       .asciiz "+"
pn_minus:      .asciiz "minus"
ps_minus:      .asciiz "-"
pn_add1:       .asciiz "add1"
pn_sub1:       .asciiz "sub1"

primitive_end: .byte   0
.align 4

###############################
# Jump Tables
###############################
print_jumps:   .word TYPE.EMPTY_LIST, print_null, TYPE.PAIR, print_list
               .word TYPE.INT, print_integer, TYPE.FIXNUM, print_fixnum
               .word TYPE.FLONUM, print_flonum, TYPE.RATIONAL, print_rational
               .word TYPE.COMPLEX, print_complex, TYPE.CHAR, print_character
               .word TYPE.STRING, print_string_object, TYPE.SYMBOL, print_symbol
               .word TYPE.BOOLEAN, print_boolean, TYPE.VECTOR, print_vector
               .word TYPE.LAMBDA, print_function, TYPE.PRIMITIVE, print_built_in
               .word TYPE.SPECIAL, print_special, TYPE.COMMENT, print_comment
               .word TYPE.ERROR, print_error

form_jumps:    .word n_eval, eval, n_apply, apply, n_define, eval_define
               .word n_set, eval_set, n_set_car, eval_set_car, n_set_cdr, eval_set_cdr
               .word n_cons, eval_cons, n_car, eval_car, n_cdr, eval_cdr
               .word n_lambda, eval_lambda, n_let, eval_let, n_let_star, eval_let_star
               .word n_letrec, eval_letrec, n_begin, eval_begin
               .word n_if, eval_if, n_cond, eval_cond, n_case, eval_case
               .word n_quote, eval_quote, n_quasiquote, eval_quasiquote
               .word 0

prim_jumps:
               .word pn_plus, prim_plus, ps_plus, prim_plus
               .word pn_minus, prim_minus, ps_minus, prim_minus
               .word pn_add1, prim_add1, pn_sub1, prim_sub1
               .word 0


eval_jumps:    .word TYPE.EMPTY_LIST, self_eval, TYPE.PAIR, eval_list
               .word TYPE.INT, self_eval, TYPE.FIXNUM, self_eval
               .word TYPE.FLONUM, self_eval, TYPE.RATIONAL, self_eval
               .word TYPE.COMPLEX, self_eval, TYPE.CHAR, self_eval
               .word TYPE.STRING, self_eval, TYPE.SYMBOL, eval_symbol
               .word TYPE.BOOLEAN, self_eval, TYPE.VECTOR, eval_vector
               .word TYPE.LAMBDA, eval_function, TYPE.SPECIAL, eval_special
               .word TYPE.PRIMITIVE, self_eval, TYPE.FORM, self_eval
               .word TYPE.COMMENT, skip_comment, TYPE.ERROR, eval_error



###############################
# General String Constants
###############################
intro:        .asciiz "Welcome to EMECHS Scheme\n"
prompt:       .asciiz "> "
errtype:      .asciiz " *UNSUPPORTED-TYPE* "
fatal:        .asciiz "Stop the world, I wanna get off!"
form_desc:    .asciiz "#[special-form "
form_end:     .asciiz "]#"
newline:      .asciiz "\n"
tab:          .asciiz "\t"
space_str:    .ascii "#"
              .byte   0x5c
              .asciiz "space"
newline_str:  .ascii "#"
              .byte   0x5c
              .asciiz "newline"
tab_str:      .ascii  "#"
              .byte   0x5c
              .asciiz "tab"
null_str:     .asciiz "()"
true_str:     .asciiz "#t"
false_str:    .asciiz "#f"
dot_str:      .asciiz " . "
.align 4

char_out:     .ascii  "#"
              .byte   0x5c
char_buffer:  .space  1
              .byte   00
.align 4


################################################################################
.text

.globl main

###############################
# main()
# entry point, welcome banner and Read-Eval-Print Loop
###############################
main:
    li $v0, print_string
    la $a0, intro
    syscall
    # initialize the heap and swap pointers
    # set aside a single nil value at the beginning of the heap and swap
    la $t0, heap
    la $t1, heap_pointer
    sw $t0, 0($t1)
    la $t0, swap
    la $t1, swap_pointer
    sw $t0, 0($t1)
    
    # save the original stack position so the stack can be munged on an exception
    la $t0, escape_frame              
    sw $sp, 0($t0)
    
main.repl:
    li $v0, print_string
    la $a0, prompt
    syscall
    nop

    la $a0, input_buffer  # give the starting position in the input buffer
    jal read_line
    nop
    move $a0, $v1
    la $a1, TOP_ENV
    jal eval
    nop

    move $a0, $v0
    jal print_object
    nop
    li $v0, print_string
    la $a0, newline
    syscall

    j main.repl
    nop
    j exit
    nop

###############################
# (char*, object*) read_line(char*)
# Read a line of text into the input buffer and parse it
###############################
read_line:
    addi $sp, $sp, -12
    sw $fp, 0($sp)
    addi $fp, $sp, 0
    sw $ra, fp.ra($fp)
    sw $a0, fp.a0($fp)  # a0 == pointer to current position in input buffer

    li $v0, read_string
    syscall
    lw $a0, fp.a0($fp)
    nop
    jal parse_object
    nop

    lw $a0, fp.a0($fp)
    lw $ra, fp.ra($fp)
    lw $fp, 0($sp)
    jr $ra
    addi $sp, $sp, 12

###############################
# (char*, object*) parse_object(char*)
# Read in an object and determine it's type
###############################
parse_object:
    addi $sp, $sp, -12
    sw   $fp, 0($sp)
    addi $fp, $sp, 0    # set the frame ptr
    sw $ra, fp.ra($fp)
    sw $a0, fp.a0($fp)  # a0 == pointer to current position in input buffer

    jal eat_whitespace
    nop

    move $t0, $v0
    # test the type of object by checking the first char
    move $t2, $zero
    lbu $t2, 0($t0)

    # test for numbers first
    move $t1, $zero    # make sure that the upper half of $t1 is cleared
    li $t1, '0'        # if less than ASCII zero, not a positive number
    blt $t2, $t1, parse_object.neg_test
    nop
    li $t1, '9'        # if greater than ASCII nine, not a positive number
    bgt $t2, $t1, parse_object.symbol_test
    nop
    move $a0, $t0
    move $a1, $t2
    jal parse_number
    nop
    j parse_object.exit
    nop

    # test for a possible negative number
parse_object.neg_test:
    li $t1, '-'                       # does the object begin with a negative sign
    bne $t2, $t1, parse_object.string_test
    nop
    # look ahead one and see if it is a number
    lbu $t3, 1($t0)
    li $t1, '0'        # if less than ASCII zero, not a negative number
    blt $t3, $t1, parse_object.symbol_test
    nop
    li $t1, '9'        # if greater than ASCII nine, not a negative number
    bgt $t3, $t1, parse_object.symbol_test

    addi $a0, $t0, 1
    jal parse_number
    move $a1, $t1

    #negate the resulting value in the object
    lw $t2, object.integer($v1)
    nop
    neg $t2, $t2
    sw $t2, object.integer($v1)

    j parse_object.exit
    nop

    # test for the start of a string
parse_object.string_test:
    li $t1, 0x22                      # determine if current char is a double-quote
    bne $t2, $t1, parse_object.hash_test
    nop
    jal parse_string
    nop
    b parse_object.exit
    nop

parse_object.hash_test:               # determine if current char is a '#'
    li $t1, 0x23
    bne $t2, $t1, parse_object.lparen_test
    nop
    lbu $t2, 1($t0)
    li $t1, 0x0000005c
    bne $t2, $t1, parse_object.true_test
    nop
    jal parse_char
    addi $a0, $t0, 2
    b parse_object.exit
    nop

parse_object.true_test:
    li $t1, 't'
    bne $t2, $t1, parse_object.false_test
    li $t3, ' '
    lbu $t2, 2($t0)
    nop
    ble $t3, $t2, parse_object.true_rparen_test
    li $t4, ')'
    addi $v0, $t0, 2
    la $v1, true
    b parse_object.exit
    nop
parse_object.true_rparen_test:
    bne $t4, $t2, parse_object.error
    nop
    move $v0, $t0
    la $v1, true
    b parse_object.exit
    nop

parse_object.false_test:
    li $t1, 'f'
    bne $t2, $t1, parse_object.error
    li $t3, ' '
    lbu $t2, 2($t0)
    nop
    ble $t3, $t2, parse_object.false_rparen_test
    li $t4, ')'
    move $v0, $t0
    la $v1, false
    b parse_object.exit
    nop
parse_object.false_rparen_test:
    bne $t4, $t2, parse_object.error
    nop
    move $v0, $t0
    la $v1, false
    b parse_object.exit
    nop


parse_object.lparen_test:
    li $t1, '('
    bne $t2, $t1, parse_object.special_test
    nop
    addi $a0, $t0, 1
    jal parse_list
    nop
    b parse_object.exit
    nop

parse_object.special_test:
    li $t1, '.'              # test for dot operator
    bne $t1, $t2, parse_object.quote_test


    b parse_object.exit
    nop

parse_object.symbol_test:
    jal parse_symbol
    move $a0, $t0
    b parse_object.exit
    nop

parse_object.quote_test:

parse_object.error:
    li $t1, err.unknown_object_type

parse_object.exit:
    lw $ra, fp.ra($fp)
    lw $fp, 0($sp)
    jr $ra
    addi $sp, $sp, 12



###############################
# (char*, object*) parse_number(char*, char)
# parse a number and store it in an object
###############################
parse_number:
    addi $sp, $sp -8
    sw $fp, 0($sp)
    addi $fp, $sp, 0
    sw $ra, fp.ra($fp)

    move $t0, $zero
    move $t4, $zero
    li $t5, 10
    li $t1, '0'        # ASCII zero
    li $t2, '9'        # ASCII nine

    # get the length of the integral part of the number
parse_number.int_get:
    lbu  $t0, 0($a0)
    addi $a0, $a0, 1
    blt $t0, $t1, parse_number.parse_int     # less than '0', continue
    nop

    bgt $t0, $t2, parse_number.parse_int     # more than '9', continue
    nop
    mult $t4, $t5
    mflo $t4
    sub $t3, $t0, $t1                        # get the value of the digit

    b parse_number.int_get
    add $t4, $t4, $t3

parse_number.parse_int:
    li $t1, '-'
    bne $a1, $t1, parse_number.positive
    nop
    neg $t4, $t4                   # subtract by zero to get the negative value
parse_number.positive:
    jal make_object
    nop
    li $t0, TYPE.INT
    sw $t0, object.type($v0)
    sw $t4, object.integer($v0)

parse_number.return:
    move $v1, $v0
    move $v0, $a0
    lw $ra, fp.ra($fp)
    lw $fp, 0($sp)
    jr $ra
    addi $sp, $sp, 8


###############################
# (char*, object*) parse_string(char*)
# parse a string object
###############################
parse_string:
    addi $sp, $sp -20
    sw $fp, 12($sp)
    addi $fp, $sp, 12
    sw $ra, fp.ra($fp)
    sw $s0, fp.s0($fp)
    sw $s1, fp.s1($fp)
    lw $s2, fp.s2($fp)

    addi $s0, $a0, 1
    jal string_length
    addi $a0, $a0, 1

    move $s2, $v0
    move $t0, $s0     # swap $a0 <-> $s0, in order to save the curr string ptr
    move $s0, $a0
    move $a0, $t0
    jal make_string
    move $a1, $v0

    move $s1, $v0
    jal make_object            # make a string object and set it
    move $a1, $v0              # to point to the string we created
    li $t0, TYPE.STRING
    sw $t0, object.type($v0)
    sw $s1, object.strptr($v0)
    sw $s2, object.strlen($v0)

parse_string.exit:
    move $v1, $v0
    move $v0, $s0

    lw $s2, fp.s2($fp)
    lw $s1, fp.s1($fp)
    lw $s0, fp.s0($fp)
    lw $ra, fp.ra($fp)
    lw $fp, 12($sp)
    jr $ra
    addi $sp, $sp, 20


###############################
# (char*, object*) parse_char(char*)
# parse a character object
###############################
parse_char:
    addi $sp, $sp -12
    sw $fp, 4($sp)
    addi $fp, $sp, 4
    sw $ra, fp.ra($fp)
    sw $s0, fp.s0($fp)

    move $s0, $zero                          # clear the upper half of $s0
    lbu $s0, 0($a0)
    li $t0, 'n'       # test for a possible newline literal
    beq $t0, $s0, parse_char.newline_test
    li $t0, 's'       # test for a possible space literal
    beq $t0, $s0, parse_char.space_test
    li $t0, 't'       # test for a possible tab literal
    beq $t0, $s0, parse_char.tab_test
    nop
parse_char.make_char:
    addi $a0, $a0, 1
    lbu $t0, 0($a0)
    li $t1, ')'
    beq $t0, $t1, parse_char.rparen
    li $t1, ' '
    bgt $t0, $t1, parse_char.error
    nop
parse_char.rparen:
    jal make_object
    nop
    li $t0, TYPE.CHAR
    sw $t0, object.type($v0)
    sb $s0, object.char($v0)
    move $v1, $v0
    b parse_char.exit


parse_char.newline_test:
    move $t3, $zero
    lbu $t3, 1($a0)
    li $t2, ' '        # check to see if there is a space next
    bge $t2, $t3, parse_char.make_char
    nop
    addi $a0, $a0, 1
    la $a1, newline_str
    addi $a1, $a1, 3
    li $a2, 6           # maximum size of the string to be tested
    jal substring_match
    nop
    move $a0, $v1
    beqz $v0, parse_char.error
    li $s0, 0x0a
    b parse_char.make_char
    nop

parse_char.space_test:
    move $t3, $zero
    lbu $t3, 1($a0)
    li $t2, ' '        # check to see if there is a space next
    bge $t2, $t3, parse_char.make_char
    nop
    addi $a0, $a0, 1
    la $a1, space_str
    addi $a1, $a1, 3
    li $a2, 4           # maximum size of the string to be tested
    jal substring_match
    nop
    move $a0, $v1
    beqz $v0, parse_char.error
    li $s0, ' '
    b parse_char.make_char

parse_char.tab_test:
    move $t3, $zero
    lbu $t3, 1($a0)
    li $t2, ' '         # check to see if there is a space next
    bge $t2, $t3, parse_char.make_char
    nop
    addi $a0, $a0, 1
    la $a1, tab_str
    addi $a1, $a1, 3
    li $a2, 2           # maximum size of the string to be tested
    jal substring_match
    nop
    move $a0, $v1
    beqz $v0, parse_char.error
    li $s0, 0x09
    b parse_char.make_char

parse_char.error:
    jal make_error
    nop
parse_char.exit:
    lw $s0, fp.s0($fp)
    lw $ra, fp.ra($fp)
    lw $fp, 4($sp)
    jr $ra
    addi $sp, $sp, 12


###############################
# (char*, object*) parse_list(char*)
# parse a list pair object
###############################
parse_list:
    addi $sp, $sp -24
    sw $fp, 16($sp)
    addi $fp, $sp, 16
    sw $ra, fp.ra($fp)
    sw $s0, fp.s0($fp)    # pointer to the list being created
    sw $s1, fp.s1($fp)    # pointer to the CAR of the list
    sw $s2, fp.s2($fp)    # pointer to the CDR of the list
    sw $s3, fp.s3($fp)    # current input pointer

    jal eat_whitespace
    nop                   # since $a0 is already the current ptr, do nothing

    move $s3, $v0
    lbu $t0, 0($s3)
    li $t1, ')'           # test for a null object
    bne $t0, $t1, parse_list.get_car
    nop

    la $s0, null          # return the null object pointer as the result
    b parse_list.exit
    nop

parse_list.get_car:
    li $t1, '('           # check to see if the CAR is a list
    bne $t0, $t1, parse_list.no_nesting_cdar
    nop
    jal parse_list        # parse the inner list
    addi $a0, $s3, 1

    move $s3, $v0
    move $s1, $v1         # get the nested list as the CAR

    jal parse_list        # continue the current list
    addi $a0, $s3, 1

    move $s3, $v0
    move $s2, $v1

    # finish the completed pair
    move $a0, $s1
    jal make_pair
    move $a1, $s2

    b parse_list.exit
    move $s0, $v0         # set the pointer to the pair

parse_list.no_nesting_cdar:
    jal parse_object      # parse the object to be pointed to by CAR
    move $a0, $s3

    move $s3, $v0
    move $s1, $v1         # save the CAR pointer
    la $s2, null

parse_list.set_car:
    # make our initial pair

    move $a0, $s1
    jal make_pair
    move $a1, $s2

    move $s0, $v0         # set the pointer to the pair

    # move on to parse the CDR
    lbu $t0, 0($s3)
    li $t1, ')'           # test for the end of the list, take one
    beq $t0, $t1, parse_list.close_list
    nop

    jal eat_whitespace
    addi $a0, $s3, -1     # eat the whitespace and test for the end of the list again

    move $s3, $v0
    lbu $t0, 0($s3)
    li $t1, ')'           # test for the end of the list, take two
    bne $t0, $t1, parse_list.get_cdr
    nop

parse_list.close_list:
    # set the endpoint for the list
    la $s2, null
    sw $s2, object.cdr($s0)
    b parse_list.exit
    nop

parse_list.get_cdr:
    li $t1, '.'
    beq $t0, $t1, parse_list.improper_list
    nop

    li $t1, '('
    bne $t0, $t1, parse_list.no_nesting_cadr
    nop
    jal eat_whitespace
    addi $a0, $s3, -1
    move $s3, $v0
    li $t1, ')'
    lbu $t4, 0($s3)
    bne $t4, $t1, parse_list.nesting_cadr
    nop
    # set the endpoint for the list
    la $s2, null
    sw $s2, object.cdr($s0)
    b parse_list.exit
    nop

parse_list.nesting_cadr:
    # the second element is a list itself, so, handle this case recursively
    # start by creating a new pair that will be the CADR of the parent list
    la $a0, null
    jal make_pair
    move $a1, $a0

    # make the new pair the CDR of the first pair
    move $s2, $v0
    sw $s2, object.cdr($s0)

    jal parse_object         # parse the contents of the list and use
    move $a0, $s3            # the list pointer as the CAR of a new pair

    move $s3, $v0
    sw $v1, object.car($s2)

    # move on to parse the CDDR
    jal eat_whitespace
    addi $a0, $s3, 0
    jal parse_list
    move $a0, $v0

    move $s3, $v0            # get the current input pointer
    sw $v1, object.cdr($s2)  # store the returned object as the CDDR

    b parse_list.exit
    nop

parse_list.no_nesting_cadr:
    jal parse_list           # recurse to get the CDR
    addi $a0, $s3, -1

    move $s3, $v0
    sw $v1, object.cdr($s0)

    b parse_list.exit
    nop

parse_list.improper_list:
    jal parse_object
    addi $a0, $s3, 1

    move $s3, $v0
    sw $v1, object.cdr($s0)

parse_list.exit:
    move $v0, $s3
    move $v1, $s0

    lw $s3, fp.s3($fp)
    lw $s2, fp.s2($fp)
    lw $s1, fp.s1($fp)
    lw $s0, fp.s0($fp)
    lw $ra, fp.ra($fp)
    lw $fp, 16($sp)
    jr $ra
    addi $sp, $sp, 24




###############################
# (char*, object*) parse_symbol(char*)
# parse a symbol object, and determine if it is a built-in
###############################
parse_symbol:
    addi $sp, $sp -36
    sw $fp, 28($sp)
    addi $fp, $sp, 28
    sw $ra, fp.ra($fp)
    sw $s0, fp.s0($fp)                # current character pointer
    sw $s1, fp.s1($fp)                # pointer to the new object
    sw $s2, fp.s2($fp)                # pointer to the builtin, or the new string
    sw $s3, fp.s3($fp)                # length of the current string
    sw $s4, fp.s4($fp)                # type of built-in currenly tested
    sw $s5, fp.s5($fp)                # counter for loops on built-ins
    sw $s6, fp.s6($fp)                # set of forms and/or primitives to match against
    
    jal symbol_length
    move $s0, $a0
    move $s3, $v0

    la $a2, special_forms             # prepare the test for special forms
    li $s4, TYPE.FORM
    move $s5, $zero
    la $s6, form_jumps
parse_symbol.builtin_test:
    move $a0, $s0
    jal test_builtin                  # test to see if a given symbol is actually a built-in form 
    move $a1, $s3
    
    move $s2, $v1
    beqz $v0, parse_symbol.not_form
    nop

parse_symbol.builtin:
    jal make_object                   # make a builtin object and set it to point to the form
    nop
    move $s1, $v0
    sw $s4, object.type($s1)
    sw $s2, object.formname($s1)
    move $a0, $s2
    jal match_builtin
    move $a1, $s6

    sw $v0, object.function($s1)

    b parse_symbol.exit
    move $v1, $s1

parse_symbol.not_form:
    bnez $s5, parse_symbol.not_builtin   # if we've gone around twice, it's not a built-in
    nop
    la $a2, primitive_forms              # prepare the test for primitive forms
    li $s4, TYPE.PRIMITIVE
    la $s6, prim_jumps
    b parse_symbol.builtin_test
    addi $s5, $s5, 1
    
parse_symbol.not_builtin:
    move $a0, $s0
    jal make_string
    move $a1, $s3

    move $s2, $v0                     # save the pointer to the new string

    jal make_object                   # make a string object and set it
    nop
    move $s1, $v0                     # to point to the string we created
    li $t0, TYPE.SYMBOL
    sw $t0, object.type($s1)
    sw $s2, object.symname($s1)
    sw $zero, object.symval($s1)
    move $v1, $v0


parse_symbol.exit:
    add $v0, $s0, $s3                 # advance the buffer pointer past the current symbol
    add $v0, $v0, 1

    lw $s5, fp.s5($fp)
    lw $s4, fp.s4($fp)
    lw $s3, fp.s3($fp)
    lw $s2, fp.s2($fp)
    lw $s1, fp.s1($fp)
    lw $s0, fp.s0($fp)
    lw $ra, fp.ra($fp)
    lw $fp, 28($sp)
    jr $ra
    addi $sp, $sp, 36



###############################
# void print_object(object*)
# determine the type of an object and dispatch on the approriate print function
###############################
print_object:
    addi $sp, $sp -8
    sw $fp, 0($sp)
    addi $fp, $sp, 0
    sw $ra, fp.ra($fp)

    lw $t0, object.type($a0)
    la $t1, print_jumps
    li $t3, TYPE.ERROR        # final object type in list
print_object.loop:
    lw $t2, 0($t1)            # load the next listing in jump table
    nop
    beq $t2, $t3, print_error # if reached the end of the list, print error
    nop
    bne $t2, $t0, print_object.loop  # not a match, try the next entry
    addi $t1, $t1, print_jumps.entry_size

    lw $t4, -4($t1)
    nop
    jalr $t4
    nop

    lw $ra, fp.ra($fp)
    lw $fp, 0($sp)
    jr $ra
    addi $sp, $sp, 8


###############################
# void print_null()
###############################
print_null:
    la $a0, null_str
    li $v0, print_string
    syscall
    nop
    jr $ra
    nop


###############################
# void print_list(object*)
###############################
print_list:
    addi $sp, $sp, -16
    sw $fp, 8($sp)
    addi $fp, $sp, 8
    sw $ra, fp.ra($fp)
    sw $s0, fp.s0($fp)             # pointer to the object's CAR
    sw $s1, fp.s1($fp)             # pointer to the object's CDR

    move $s0, $a0                  # start by printing an opening paren
    li $a0, '('
    li $v0, print_char
    syscall

    lw $a0, object.car($s0)        # print the CAR of the pair
    nop
    jal print_object
    nop

    lw $s1, object.cdr($s0)        # get the object pointed to by CDR
    nop

print_list.cdr_loop:
    lw $t0, object.type($s1)       # get the object type of the CDR
    li $t1, TYPE.EMPTY_LIST
    beq $t0, $t1, print_list.end_list

print_list.print_pair:
    li $t1, TYPE.PAIR
    beq $t0, $t1, print_list.continued_list
    nop

print_list.print_improper_list:
    move $s0, $a0                  # if the next object is neither a null
    la $a0, dot_str                # nor a pair, then print a dot before
    li $v0, print_string           # it to show that it is an improper list
    syscall
    move $a0, $s1
    jal print_object
    nop
    b print_list.end_list
    nop

print_list.continued_list:
    li $a0, ' '
    li $v0, print_char             # print one space between CAR and CDR
    syscall

print_list.display_cdr:
    lw $a0, object.car($s1)        # get the next pair's CAR
    jal print_object
    nop
    lw $t2, object.type($s1)
    li $t1, TYPE.EMPTY_LIST
    beq $t2, $t1, print_list.end_list

    lw $t3, object.cdr($s1)        # get the next pair's CDR, and loop
    nop
    b print_list.cdr_loop
    move $s1, $t3

print_list.end_list:
    li $a0, ')'
    li $v0, print_char
    syscall

print_list.exit:
    lw $s1, fp.s1($fp)
    lw $s0, fp.s0($fp)
    lw $ra, fp.ra($fp)
    lw $fp, 8($sp)
    jr $ra
    addi $sp, $sp, 16


###############################
print_integer:
    move $t0, $a0
    lw $a0, object.integer($t0)
    li $v0, print_int
    syscall
    nop
    jr $ra
    nop

###############################
print_fixnum:
    j print_error
    nop
###############################
print_flonum:
    j print_error
    nop
###############################
print_rational:
    j print_error

###############################
print_complex:
    j print_error
    nop


###############################
# void print_character(object*)
###############################
print_character:
    li $v0, print_string       # initialize the argument for the syscall

    la $t0, char_out
    move $t1, $zero
    lb $t1, object.char($a0)

    li $t2, 0x0a
    beq $t1, $t2, print_character.newline
    li $t2, 0x20
    beq $t1, $t2, print_character.space
    li $t2, 0x09
    beq $t1, $t2, print_character.tab

    sb $t1, 2($t0)
    b print_character.exit
    move $a0, $t0

print_character.newline:
    la $a0, newline_str
    b print_character.exit
    nop

print_character.space:
    la $a0, space_str
    b print_character.exit
    nop

print_character.tab:
    la $a0, tab_str

print_character.exit:
    syscall
    nop
    jr $ra
    nop

###############################
# void print_string_object(object*)
###############################
print_string_object:
    move $t0, $a0
    li $a0, 0x22
    li $v0, print_char
    syscall
    nop
    lw $a0, object.strptr($t0)
    li $v0, print_string
    syscall
    nop
    li $a0, 0x22
    li $v0, print_char
    syscall
    nop
    jr $ra
    nop

###############################
print_symbol:
    move $t0, $a0
    lw $a0, object.symname($t0)
    li $v0, print_string
    syscall
    nop

    jr $ra
    nop

###############################
print_function:
    move $t0, $a0
    lw $a0, object.formname($t0)
    li $v0, print_string
    syscall
    nop

    jr $ra
    nop

###############################
print_built_in:
    move $t0, $a0
    la $a0, form_desc
    li $v0 print_string
    syscall
    nop

    lw $a0, object.formname($t0)
    li $v0, print_string
    syscall
    nop

    la $a0, form_end
    li $v0 print_string
    syscall
    nop

    jr $ra
    nop


###############################
# void print_boolean(object*)
###############################
print_boolean:
    lw $t0, object.boolval($a0)
    nop
    beqz $t0, print_boolean.false
    nop
    la $a0, true_str
    b print_boolean.display
    nop

print_boolean.false:
    la $a0, false_str

print_boolean.display:
    li $v0, print_string
    syscall
    nop

    jr $ra
    nop


###############################
#
###############################
print_special:
    move $t0, $a0
    lbu $a0, object.special($t0)
    li $v0, print_char
    syscall
    nop
    jr $ra
    nop



###############################
print_comment:
    j print_error


###############################
print_vector:
    j print_error

    
    
    
###############################
print_error:
    la $a0, errtype
    li $v0, print_string
    syscall

    jr $ra
    nop

###############################
make_error:
###############################
    j fatal_error

###############################
# object* make_object(void)
# allocate space for an object
###############################
make_object:
    addi $sp, $sp -16
    sw $fp, 8($sp)
    addi $fp, $sp, 8
    sw $ra, fp.ra($fp)
    sw $s0, fp.s0($fp)
    sw $s1, fp.s1($fp)

    la $s0, heap_pointer
    lw $s1, 0($s0)                       # get the current global heap pointer
    la $t1, swap
    addi $t0, $s1, object.sizeof

    bgt $t1, $t0, make_object.allocate   # if adding a new object would overrun
                                         # the heap, run the GC
    nop
    jal collect_garbage
    nop
    move $t0, $v0       # get new heap pointer returned by collect_garbage
make_object.allocate:
    sw $t0, 0($s0)      # store the updated heap pointer
    sw $zero, 0($s1)    # clear the old data from the new object
    sw $zero, 4($s1)
    sw $zero, 8($s1)
    sw $zero, 12($s1)
    move $v0, $s1       # return the address of the new object

    lw $s1, fp.s1($fp)
    lw $s0, fp.s0($fp)
    lw $ra, fp.ra($fp)
    nop
    lw $fp, 8($sp)
    jr $ra
    addi $sp, $sp, 16



###############################
# char* make_string(char*, int)
###############################
make_string:
    addi $sp, $sp, -24
    sw $fp, 16($sp)
    addi $fp, $sp, 16
    sw $ra, fp.ra($fp)
    sw $s0, fp.s0($fp)                # global heap pointer
    sw $s1, fp.s1($fp)                # size of padding to add to the string
    sw $s2, fp.s2($fp)                # pointer to the global heap pointer
    sw $s3, fp.s3($fp)                # pointer to the endpoint of the new string

    li $t0, 4
    la $s2, heap_pointer
    lw $s0, 0($s2)                    # get the current heap pointer
    la $t3, swap                      # get the endpoint of the heap
    addi $t3, $t3, -1
    add $t2, $s0, $a1                 # get the heap pointer plus the size of the string
                                      # to determine the endpoint of the new string

    addi $s3, $t2, 1                  # add one extra character for the zero-delimiter,
                                      # needed by print_string
    divu $s3, $t0
    mfhi $t5
    sub $s1, $t0, $t5                 # get the size of the offset used to word-align the data
    add $t4, $s3, $s1
    bgt $t3, $t4, make_string.allocate
    nop
    jal collect_garbage
    nop
    move $t4, $v0

make_string.allocate:
    sw $t4, 0($s2)                    # update the heap pointer by the aligned size

    move $a2, $a1                     # copy the string to the allocated space
    move $a1, $s0
    jal string_copy
    nop

    move $a0, $v1
    move $a1, $zero
    jal fill_string
    move $a2, $s1

make_string.exit:
    sb $zero, -1($s3)                  # force delimit the string in case of a paren
    move $v0, $s0                      # calculate the current pointer position

    lw $s3, fp.s3($fp)
    lw $s2, fp.s2($fp)
    lw $s1, fp.s1($fp)
    lw $s0, fp.s0($fp)
    lw $ra, fp.ra($fp)
    lw $fp, 16($sp)
    jr $ra
    addi $sp, $sp, 24



###############################
# object* make_pair(object*, object*)
###############################
make_pair:
    addi $sp, $sp -16
    sw $fp, 8($sp)
    addi $fp, $sp, 8
    sw $ra, fp.ra($fp)
    sw $s0, fp.s0($fp)
    sw $s1, fp.s1($fp)

    move $s0, $a0              # save the arguments
    move $s1, $a1              # as they may get changed by make_object

    jal make_object            # allocate on object to populate
    nop
    li $t0, TYPE.PAIR
    sw $t0, object.type($v0)
    sw $s0, object.car($v0)    # set the CAR to the first arg
    sw $s1, object.cdr($v0)    # and the CDR to the second

    lw $s1, fp.s1($fp)
    lw $s0, fp.s0($fp)
    lw $ra, fp.ra($fp)
    nop
    lw $fp, 8($sp)
    jr $ra
    addi $sp, $sp, 16



###############################
# char* collect_garbage()
###############################
collect_garbage:
    j fatal_error



###############################
# char* eat_whitespace(char*)
# advance the buffer pointer until it reaches the next non-whitespace char
###############################
eat_whitespace:
    move $t0, $a0
    li $t1, ' '        # set $t1 to the single space character
    move $t2, $zero    # make sure that the upper half of $t2 is cleared

eat_whitespace.loop:
    lbu $t2, 0($t0)    # get the character to be tested
    nop
    ble $t2, $t1, eat_whitespace.loop
    addi $t0, $t0, 1   # advance the buffer pointer

    jr $ra
    addi $v0, $t0, -1  # set the return value to the current pointer


###############################
# (char*, char*) string_copy(char*, char*, int)
###############################
string_copy:
    move $t1, $a1
string_copy.loop:
    lbu $t0, 0($a0)
    addi $a0, $a0, 1
    sb $t0, 0($t1)
    addi $t1, $t1, 1
    bnez $a2, string_copy.loop
    addi $a2, $a2, -1

    move $v1, $t1
    jr $ra
    move $v0, $a1



###############################
# char* fill_string(char*, char, int)
###############################
fill_string:
    sb $a1, 0($a0)
    bnez $a2, fill_string
    addi $a2, $a2, -1
    jr $ra
    nop


###############################
# (boolean, char*) substring_match(char*, char*, int)
# compares two substrings and returns true if they match
# either to the end of the string or to a given
# maximum size
###############################
substring_match:
    move $t0, $zero                          # clear the upper half of $t0
    move $t1, $zero                          # clear the upper half of $t1
    add  $t2, $a2, $a0
    addi $t2, $t2, -1                        # endpoint of the tested string
substring_match.loop:
    lbu $t0, 0($a0)
    lbu $t1, 0($a1)
    nop
    bne $t0, $t1, substring_match.failure    # chars done match, return false
    nop
    beqz $t0, substring_match.success        # both are at end of string, done
    nop
    bge $a0, $t2, substring_match.success    # end of the substring, done
    nop
    addi $a0, $a0, 1
    addi $a1, $a1, 1
    b substring_match.loop
    nop

substring_match.success:
    li $v0, 1
    b substring_match.exit
    nop
substring_match.failure:
    li $v0, 0
substring_match.exit:
    move $v1, $a0
    jr $ra
    nop



###############################
# int strlen(char*)
# get length of zero-delimited string
###############################
strlen:
    move $v0, $zero                   # initialize counter
    b    strlen.test
strlen.inc:
    lbu    $t0, ($a0)
    addi    $v0, 1
    addiu    $a0, 1
strlen.test:
    bnez    $t0, strlen.inc
    nop
    jr    $ra
    addi $v0, $v0, -1

###############################
# int string_length(char*)
# get length of quoted string
###############################
string_length:
    move $v0, $zero                   # initialize counter
    li   $t1, 0x22                    # $t0 == double-quote
    move $t0, $zero

string_length.inc:
    lbu    $t0, 0($a0)
    addi    $v0, $v0, 1
    addiu    $a0, $a0, 1
string_length.test:
    bne    $t0, $t1, string_length.inc
    nop
    jr    $ra
    addi $v0, $v0, -1


###############################
# int symbol_length(char*)
# get length of a symbol
###############################
symbol_length:
    li    $v0, 0        # initialize counter
    li  $t1, 0x20      # $t0 == space
    move $t0, $zero
    li $t2, ')'
    li $t3, '('

symbol_length.inc:
    lbu    $t0, 0($a0)
    addi    $v0, $v0, 1
    addiu    $a0, $a0, 1
symbol_length.test:
    beq $t0, $t2, symbol_length.exit  # exit if you've reached an rparen
    nop
    beq $t0, $t3, symbol_length.exit  # exit if you've reached an lparen
    nop
    bgt    $t0, $t1, symbol_length.inc
    nop
    b symbol_length.exit
    nop

symbol_length.found_paren:
    addi $v0, $v0, -1

symbol_length.exit:
    jr    $ra
    addi $v0, $v0, -1


###############################
# (bool, char*) test_builtin(char*, int, char[][])
# test a symbol to see if it matches
# one of the special forms or primitives
###############################
test_builtin:
    addi $sp, $sp -24
    sw $fp, 16($sp)
    addi $fp, $sp, 16
    sw $ra, fp.ra($fp)
    sw $s0, fp.s0($fp)                # pointer to the string being compared
    sw $s1, fp.s1($fp)                # length of the compared string
    sw $s2, fp.s2($fp)                # pointer to the form string to test against
    sw $s3, fp.s3($fp)                # length of the compared form string

    move $s0, $a0                     # symbol to test
    move $s1, $a1                     # length of the symbol to test
    move $s2, $a2                     # start at the beginning of the list of symbols to search

test_builtin.test_loop:
    jal symbol_length                 # get the length of the current special form to test against
    move $a0, $s2

    move $s3, $v0

    bne $s3, $s1, test_builtin.iterate    #  if the strings are of different sizes, skip to the next one
    nop
    move $a0, $s0                     # else run a more comprehensive comparison
    move $a1, $s2
    jal substring_match
    move $a2, $s3

    bnez $v0, test_builtin.true       # found a match, return true
    nop

test_builtin.iterate:
    add $s2, $s2, $s3                 # advance past the end of the current symbol
    addi $s2, $s2, 1                  # including the string delimiter
    lbu $t0, 0($s2)                   # get the next byte and see if it is the zero marker
    nop
    bnez $t0, test_builtin.test_loop  # if we haven't reached the end of the list, repeat
    nop

test_builtin.false:
    move $v0, $zero
    b test_builtin.exit
    move $v1, $zero

test_builtin.true:
    li $v0, 1
    move $v1, $s2

test_builtin.exit:
    lw $s3, fp.s3($fp)
    lw $s2, fp.s2($fp)
    lw $s1, fp.s1($fp)
    lw $s0, fp.s0($fp)
    lw $ra, fp.ra($fp)
    nop
    lw $fp, 16($sp)
    jr $ra
    addi $sp, $sp, 24

###############################
# function* match_builtin(char*, char[][])
# Evaluate the input as a Scheme statement
###############################
match_builtin:
    move $t0, $a1                     # get the starting point of the forms jump table
    
match_builtin.loop:
    lw $t1, 0($t0)
    nop
    beqz $t1, match_builtin.no_match  # if you reach the end of the table, die
    move $t2, $t0
    bne $a0, $t1, match_builtin.loop
    addi $t0, $t0, 8

    lw $v0, 4($t2)
    nop
    jr $ra
    nop

match_builtin.no_match:
    j fatal_error

###############################
# object* eval(object*, environment*)
# Evaluate the input as a Scheme statement
###############################
eval:
    addi $sp, $sp -16
    sw $fp, 8($sp)
    addi $fp, $sp, 8
    sw $ra, fp.ra($fp)
    sw $s0, fp.s0($fp)                # pointer to the list being evaluated
    sw $s1, fp.s1($fp)                # pointer to the current environment

    move $s0, $a0
    move $s1, $a1

    # go through the eval dispatch table for the different evaluation strategies
    lw $t0, object.type($s0)
    la $t1, eval_jumps
    li $t3, TYPE.ERROR                # final object type in list
eval.loop:
    lw $t2, 0($t1)                    # load the next listing in jump table
    nop
    beq $t2, $t3, fatal_error         # if reached the end of the list, print error
    nop
    bne $t2, $t0, eval.loop           # not a match, try the next entry
    addi $t1, $t1, eval_jumps.entry_size

    lw $t4, -4($t1)
    move $a0, $s0
    jalr $t4
    move $a1, $s1
    
    lw $s1, fp.s1($fp)
    lw $s0, fp.s0($fp)
    lw $ra, fp.ra($fp)
    nop
    lw $fp, 8($sp)
    jr $ra
    addi $sp, $sp, 16

###############################
# object* self_eval(object*, environment*)
###############################
self_eval:
    move $v0, $a0
    jr $ra
    nop

###############################
# object* eval_list(list*, environment*)
###############################
eval_list:
    addi $sp, $sp, -28
    sw $fp, 20($sp)
    addi $fp, $sp, 20
    sw $ra, fp.ra($fp)
    sw $s0, fp.s0($fp)                # pointer to the list being evaluated
    sw $s1, fp.s1($fp)                # pointer to the current environment
    sw $s2, fp.s2($fp)                # pointer to the first evaluated result
    sw $s3, fp.s3($fp)                # pointer to the list of evaluated arguments
    sw $s4, fp.s4($fp)                # pointer to the next unevaluated argument

    move $s0, $a0
    move $s1, $a1
    
    lw $a0, object.car($s0)
    move $a1, $s1                     # redundant, but otherwise I'd have a nop here
    jal eval
    nop
    move $s2, $v0

    la $a1, null
    la $a0, null                      # prepare a list entry point for evaluated values
    jal make_pair
    nop

    lw $s4, object.cdr($s0)           # get the list of arguments
    move $s3, $v0                     
    
    
    lw $a0, object.car($s4)           # put the first argument in the car of the new list
    nop
    jal eval
    nop
    move $t0, $v0
    sw $t0, object.car($s3)
    
    li $t1, TYPE.FORM                # test for the special forms
    
    
    
    j eval_list.args_test
    nop
    
eval_list.eval_args:
    lw $t0, object.type($s4)          # validate that the list of arguments
    li $t1, TYPE.PAIR                 # actually is a proper list
    bne $t0, $t1, fatal_error
    nop

    lw $a0, object.car($s4)
    nop
    jal eval                          # evaluate the next element of the arg list
    move $a1, $s1
    
    move $a1, $v0                    
    jal destructive_append            # add the evaluated result to the list of arguments
    move $a0, $s3  
    
eval_list.args_test:
    move $t1, $s4
    lw $s4, object.cdr($t1)           # get the next evaluable element
    nop
    lw $t2, object.type($s4)          # if you haven't reached the end of the list, 
    li $t0, TYPE.EMPTY_LIST
    bne $t2, $t0, eval_list.eval_args # go on to the next argument
    
eval_list.complete_args:
    move $a0, $s2
    move $a1, $s3
    jal apply
    move $a2, $s1

    # return the return value of apply, no actual changes needed

eval_list.exit:
    lw $s4, fp.s4($fp)
    lw $s3, fp.s3($fp)
    lw $s2, fp.s2($fp)
    lw $s1, fp.s1($fp)
    lw $s0, fp.s0($fp)
    lw $ra, fp.ra($fp)
    nop
    lw $fp, 20($sp)
    jr $ra
    addi $sp, $sp, 28


eval_define:
eval_set:
eval_set_car:
eval_set_cdr:
eval_cons:
eval_car:
eval_cdr:
eval_lambda:
eval_let:
eval_let_star:
eval_letrec:
eval_begin:
eval_if:
eval_cond:
eval_case:
eval_quote:
eval_quasiquote:
eval_at:
eval_symbol:
eval_vector:
skip_comment:
prim_minus:
prim_sub1:
    j fatal_error
    nop


###############################
# object* eval_plus(list*, environment*)
###############################
prim_plus:
    addi $sp, $sp -16
    sw $fp, 8($sp)
    addi $fp, $sp, 8
    sw $ra, fp.ra($fp)
    sw $s0, fp.s0($fp)                # pointer to the list being evaluated
    sw $s1, fp.s1($fp)                # pointer to the current environment

    move $s0, $a0
    move $s1, $zero

prim_plus.loop:
    lw $t1, object.car($a0)
    nop
    lw $t2, object.cdr($a0)
    nop
    lw $t3, object.car($t2)
    nop

    add $s1, $t1, $t3
    jal make_object
    nop

    li $t4, TYPE.FIXNUM
    sw $t4, object.type($v0)
    sw $s1, object.integer($v0)

prim_plus.exit:
    lw $s1, fp.s1($fp)
    lw $s0, fp.s0($fp)
    lw $ra, fp.ra($fp)
    nop
    lw $fp, 8($sp)
    jr $ra
    addi $sp, $sp, 16


###############################
# object* eval_add1(list*, environment*)
###############################
prim_add1:
    addi $sp, $sp -16
    sw $fp, 8($sp)
    addi $fp, $sp, 8
    sw $ra, fp.ra($fp)
    sw $s0, fp.s0($fp)                # pointer to the value to be added one to
    sw $s1, fp.s1($fp)                # final result after adding one

    move $s0, $a0
    move $s1, $zero
        
    lw $t1, object.car($a0)           # strip the argument out of the surrounding list
    nop
    
    lw $t2, object.type($t1)
    li $t3, TYPE.INT
    bne $t2, $t3, prim_add1.error
    nop
    lw $t2, object.integer($t1)       # get the actual value
    nop

    addi $s1, $t2, 1                  # perform the actual addition operation   
    jal make_object                   # make an object to hold the result
    nop

    li $t3, TYPE.INT                  
    sw $t3, object.type($v0)
    sw $s1, object.integer($v0)
    j prim_add1.exit                  # bypass the error case
    
prim_add1.error:
    la $v0, false
    
prim_add1.exit:
    lw $s1, fp.s1($fp)
    lw $s0, fp.s0($fp)
    lw $ra, fp.ra($fp)
    lw $fp, 8($sp)
    jr $ra
    addi $sp, $sp, 16


###############################
# object* apply(object*, list*, environment*)
###############################
apply:
    addi $sp, $sp -8
    sw $fp, 0($sp)
    addi $fp, $sp, 0
    sw $ra, fp.ra($fp)

    lw $t0, object.type($a0)
    li $t1, TYPE.PRIMITIVE
    bne $t0, $t1, apply.test_function
    nop
    # go through the eval dispatch table for the different evaluation strategies
    lw $t0, object.function($a0) 
    
    move $a0, $a1
    jalr $t0
    move $a1, $a2
    b apply.exit
    nop

apply.test_function:
    li $t1, TYPE.LAMBDA
    b fatal_error

apply.exit:
    lw $ra, fp.ra($fp)
    lw $fp, 0($sp)
    jr $ra
    addi $sp, $sp, 8



###############################
# list* destructive_append(list*, object*)
###############################
destructive_append:
    addi $sp, $sp -8
    sw $fp, 0($sp)
    addi $fp, $sp, 0
    sw $ra, fp.ra($fp)
    
    move $t0, $a0
    lw $t1, object.type($t0) 
    li $t2, TYPE.PAIR
    bne $t2, $t1, fatal_error          # not a proper list, raise an error    
    nop
    lw $t3, object.cdr($t0)
    j destructive_append.seek_test
    nop
    
destructive_append.seek_next:
    li $t2, TYPE.PAIR
    bne $t2, $t1, fatal_error          # not a proper list, raise an error

    move $t3, $t0
    lw $t0, object.cdr($t3)
    nop
destructive_append.seek_test:
    lw $t1, object.type($t0)
    li $t2, TYPE.EMPTY_LIST            # if you haven't reached the end of the list, continue
    bne $t2, $t1, destructive_append.seek_next    
    nop
    
destructive_append.complete_list:
    sw $a1, object.cdr($t3)            # replace the existing reference to the null list
    move $v0, $a0                      # with the new inserted object
    
destructive_append.exit:
    lw $ra, fp.ra($fp)
    lw $fp, 0($sp)
    jr $ra
    addi $sp, $sp, 8    

    
###############################
fatal_error:
    li $v0, print_string
    la $a0, fatal
    syscall
    nop

###############################
exit:
    li $v0, exit2
    syscall


