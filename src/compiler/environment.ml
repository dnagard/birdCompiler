(* This file contains the data structure and functions we will use to manage
   the environment as we compile our code. *)

open Batteries;;

open Assembly;;

(* This data type represents the kind of environment we will use in our
   compiler.  This is a pair between the next free stack address and a
   dictionary mapping variable names to their offsets. *)
type environment = int * argument Map.String.t;;

(* This defines an exception which should be thrown if we try to read from an
   environment something that doesn't exist. *)
exception UnboundVariable of string * environment;;

(* The value representing an empty environment.  The map is empty and zero
   temporary locations are in use. *)
let empty_environment : environment = (-8, Map.String.empty);;

(* A function that matches the name of a function to the memory location where the 
   closure is stored.  *)
let store_func_location (name : string) (env : environment) : environment =
  match env with 
  | top, dict -> 
    let newMap = Map.String.add name (ArgMemory (AddrByBirdLabel ("closure_of_"^name))) dict in 
    (top),newMap
;;

(* A function to allocate a space for a named variable from the environment.
   This function takes a variable name and an environment.  It returns the
   new environment after that allocation.
*)
let allocate_named_variable (name : string) (env : environment) : environment =
  match env with 
  | top, dict -> 
    let newMap = Map.String.add name (ArgMemory (AddrByRegisterOffset (RBP,top))) dict in 
    (top-8),newMap
;;



(* A function to find a previously-allocated space in an environment based upon
   the name of the variable for which that space was allocated.  If the variable
   was not previously allocated, then UnboundVariable is raised.
*)
let find_named_variable (name : string) (env : environment) : argument =
  match env with 
  | _, dict -> 
    begin 
      match Map.String.find_opt name dict with 
      | Some addr -> addr
      | None -> raise (UnboundVariable (name,env))
    end 
;;

(* A function to allocate space for a temporary variable from the environment.
   This function does not require a variable name because the temporary space is
   being allocated for the compiler and will not be associated with a name.
   Given an environment, it returns a new, temporary address as well as a new
   environment after that allocation.
*)
let allocate_temp_variable (env : environment) : argument * environment =
  match env with 
  | next_free, dict -> 
    (ArgMemory(AddrByRegisterOffset (RBP, next_free))), (next_free-8, dict)
;;

(* A function to print an environment.  This is provided to you for debugging
   purposes. *)
let string_of_environment (env : environment) : string =
  let (next_free, dict) = env in
  let mappings = List.of_enum (Map.String.enum dict) in
  let rec string_of_mappings ms =
    match ms with
    | [] -> ""
    | (name, address)::ms' ->
      (Printf.sprintf
         "%s stored at %s\n"
         name
         (code_of_argument address)
      ) ^ string_of_mappings ms'
  in
  (Printf.sprintf "Next free offset: %d\n" next_free) ^
  string_of_mappings mappings
;;

(* Converts 32bit to 64bit *)
let string_of_twice_int (n : int) : string =
  Int64.to_string (Int64.mul (Int64.of_int n) (Int64.of_int 2))
;;


(* Returns an integer for what the offset is for a piece of memory *)
let stack_memory_of_argument (arg : argument) :  int = 
  match arg with 
  | ArgConstant _ | ArgRegister _ -> 0
  | ArgMemory mem -> 
    match mem with 
    | AddrByRegister _ | AddrByLabel _ | AddrByBirdLabel _ -> 0 
    | AddrByRegisterOffset (reg, offset) -> 
      begin
        match reg with 
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
        | RBX
        | R12
        | R13
        | R14
        | R15 -> 0
        | RBP-> offset 
      end

;;

(* Returns the max offset for all arguments in an instruction *)
let stack_memory_of_instruction (inst : instruction) : int = 
  match inst with 
  | AsmAdd (a1, a2)
  | AsmIMul (a1, a2)
  | AsmMov (a1, a2)
  | AsmSub (a1, a2)
  | AsmShl (a1, a2)
  | AsmShr (a1, a2)
  | AsmSal (a1, a2)
  | AsmSar (a1, a2)
  | AsmAnd (a1, a2)
  | AsmOr (a1, a2)
  | AsmXor (a1, a2)
  | AsmCmp (a1, a2) -> 
    begin
      let (opt1, opt2) = (stack_memory_of_argument a1, stack_memory_of_argument a2) in 
      if opt1 <= opt2 then opt1 else opt2
    end
  |AsmPush a1 | AsmPop a1 -> stack_memory_of_argument a1
  | AsmLabel _ | AsmJmp _ | AsmJe _ | AsmJne _ | AsmJl _ | AsmJg _ | AsmRet | AsmCall _ 
  | AsmAlign _ | AsmDq _ | AsmSection _ | AsmRepMovsq | AsmRepStosq | AsmJz _ -> 0
;;

(* Returns the max offset for all instructions in a list *)
let rec stack_memory_of_instruction_list (instList : instruction list) : int =
  match instList with
  | [] -> 0
  | inst::next -> 
    let offset = stack_memory_of_instruction inst in 
    let nextOffset = stack_memory_of_instruction_list next in
    if offset <= nextOffset then offset else nextOffset
;;

(* Creates an environment where each parameter is mapped to positive offsets *)
let rec create_function_environment (p : string list) (env: environment)(offset : int) : environment =
  match p with 
  | head::tail -> 
    begin
      match env with 
      | top, dict -> 
        let newMap = Map.String.add head (ArgMemory (AddrByRegisterOffset (RBP,offset))) dict in 
        create_function_environment tail ((top),newMap) (offset + 8)
    end
  | [] -> env
;;

