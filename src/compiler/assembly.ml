(** This file contains type declarations and functions related to the compiler's
    representation of x86-64 assembly language. *)

open Batteries;;

type errors =
  | One
  | Two
  | Three
  | Four
  | Five
;;

let code_of_errors (e : errors) : string =
  match e with 
  | One -> "1"
  | Two -> "2"
  | Three -> "3"
  | Four -> "4"
  | Five -> "5"
;;


(** Describes the registers of x86-64 that our code will use. *)
type register =
  | RAX
  | RSP
  | R10
  | RDI
  | RSI
  | RDX
  | RCX
  | R8
  | R9
  | R11
  | RBP
  | RBX
  | R12
  | R13
  | R14
  | R15

;;

(** Describes a memory address expression in x86-64 assembly. *)
type address =
  | AddrByRegister of register
  | AddrByRegisterOffset of register * int
  | AddrByLabel of string
  | AddrByBirdLabel of string
;;

(** Describes the type of arguments in our x86-64 assembly representation.  We
    use this type somewhat loosely: not every argument is valid everywhere an
    argument type is written below, but capturing the precise syntax limitations
    of x86 would make our assembly language types a lot more complicated.

    Note that the string argument of ArgConstant is the textual representation
    of the constant to be emitted to the assembly file, such as "5" or
    "0xFFFFFFFFFFFFFFFE".
*)
type argument =
  | ArgConstant of string
  | ArgRegister of register
  | ArgMemory of address
;;

(** The type that represents single x86 instructions. *)
type instruction =
  | AsmAdd of argument * argument 
  | AsmIMul of argument * argument
  | AsmMov of argument * argument
  | AsmSub of argument * argument
  | AsmShl of argument * argument 
  | AsmShr of argument * argument
  | AsmSal of argument * argument
  | AsmSar of argument * argument
  | AsmAnd of argument * argument
  | AsmOr of argument * argument
  | AsmXor of argument * argument
  | AsmLabel of string 
  | AsmCmp of argument * argument
  | AsmJmp of string
  | AsmJe of string 
  | AsmJne of string 
  | AsmJl of string 
  | AsmJg of string 
  | AsmJz of string
  | AsmPush of argument
  | AsmPop of argument
  | AsmCall of string
  | AsmDq of string list 
  | AsmAlign of int 
  | AsmSection of string
  | AsmRepMovsq
  | AsmRepStosq
  | AsmRet
;;

(** A function which transforms an x86 register into a string suitable for
    writing into an assembly language file. *)

let code_of_register (register : register) : string =
  match register with 
  | RAX -> "rax"
  | RSP -> "rsp"
  | R10 -> "r10"
  | RDI -> "rdi"
  | RSI -> "rsi"
  | RDX -> "rdx"
  | RCX -> "rcx"
  | R8 -> "r8"
  | R9 -> "r9"
  | R11 -> "r11"
  | RBP -> "rbp"
  | RBX -> "rbx"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14"
  | R15 -> "r15"
;;  

(** A function which transforms an x86 address expression into a string suitable
    for writing into an assembly language file. *)
let code_of_address (address : address) : string =
  match address with
  | AddrByRegister reg -> "[" ^ code_of_register reg ^ "]"
  | AddrByRegisterOffset (reg, integer) -> "[" ^ code_of_register reg ^ " + "
                                           ^ string_of_int integer ^ "]"
  | AddrByLabel label -> "[" ^ label ^ "]" 
  | AddrByBirdLabel label -> label ^ " + 1"
;;

(** A function which transforms an x86 argument into a string suitable for
    writing into an assembly language file. *)
let code_of_argument (argument : argument) : string =
  match argument with 
  | ArgConstant str -> str
  | ArgRegister reg -> code_of_register reg
  | ArgMemory adr -> code_of_address adr
;;

(** A function which transforms an x86 instruction into a string suitable for
    writing into an assembly language file.  For example, given the input
    AsmRet, an appropriate return value might be "  ret\n".
*)
let code_of_instruction (instruction : instruction) : string =
  match instruction with 
  | AsmAdd (dest, src) -> "add " ^ code_of_argument dest ^ ", " ^ code_of_argument src ^ "\n"
  | AsmIMul (dest, src) -> "imul " ^ code_of_argument dest ^ ", " ^ code_of_argument src ^ "\n"
  | AsmMov (dest, src) -> "mov " ^ code_of_argument dest ^ ", " ^ code_of_argument src ^ "\n"
  | AsmSub (dest, src) -> "sub " ^ code_of_argument dest ^ ", " ^ code_of_argument src ^ "\n"
  | AsmShl (dest, shift) -> "shl " ^ code_of_argument dest ^ ", " ^ code_of_argument shift ^"\n"
  | AsmShr (dest, shift) -> "shr " ^ code_of_argument dest ^ ", " ^ code_of_argument shift ^ "\n"
  | AsmSal (dest, shift) -> "sal " ^ code_of_argument dest ^ ", " ^ code_of_argument shift ^ "\n"
  | AsmSar (dest, shift) -> "sar " ^ code_of_argument dest ^ ", " ^ code_of_argument shift ^ "\n"
  | AsmAnd (dest, src) -> "and " ^ code_of_argument dest ^ ", " ^ code_of_argument src ^ "\n"
  | AsmOr (dest, src) -> "or " ^ code_of_argument dest ^ ", " ^ code_of_argument src ^ "\n"
  | AsmXor (dest, src) -> "xor " ^ code_of_argument dest ^ ", " ^ code_of_argument src ^ "\n"
  | AsmLabel string -> string ^ ":\n"
  | AsmCmp (dest, src) -> "cmp " ^ code_of_argument dest ^ ", " ^ code_of_argument src ^ "\n"
  | AsmJmp string -> "jmp " ^ string ^ "\n"
  | AsmJe string -> "je " ^ string ^ "\n"
  | AsmJne string -> "jne " ^ string ^ "\n"
  | AsmJl string -> "jl " ^ string ^ "\n"
  | AsmJg string -> "jg " ^ string ^ "\n"
  | AsmJz string -> "jz " ^ string ^ "\n"
  | AsmPop arg -> "pop " ^ code_of_argument arg ^ "\n"
  | AsmPush arg -> "push " ^ code_of_argument arg ^ "\n"
  | AsmCall str -> "call " ^ str ^ "\n"
  | AsmAlign int -> "align " ^ string_of_int int ^"\n"  
  | AsmDq stringList -> "dq " ^ (String.concat ", " stringList) ^ "\n"
  | AsmSection string -> "section ." ^ string ^ "\n"
  | AsmRepMovsq -> "rep movsq\n"
  | AsmRepStosq -> "rep stosq\n"
  | AsmRet -> "ret\n"
;;

(** A function which transforms a list of x86 instructions into a string
    suitable for writing into an assembly language file. *)
let rec code_of_instruction_list (instruction_list : instruction list) : string =
  match instruction_list with 
  | [] -> ""
  | instruction::next -> code_of_instruction instruction ^ code_of_instruction_list next 
;;
