(** This file contains the definition of the compiler: the tool that translates
       an AST in our language into assembly language. *)
open Batteries;;
open HatchLanguage;;
open Freshening;;
open Assembly;;
open Asts;;
open Environment;;
open Printf;;
open Wellformedness;;


(*These were used a lot in previous labs, but keeping them here in case they become useful again. 
   They were once used in the check_rax function.*)
let true_string = "0xFFFFFFFFFFFFFFFF";;
let false_string = "0x7FFFFFFFFFFFFFFF";;

let bool_checker = "0x3FFFFFFFFFFFFFFF";;
let pointer_checker = "0xBFFFFFFFFFFFFFFF";; 
let closure_checker = "0x8000000000000000";;

(* Checks if the values stored in RAX are ints and stops with error otherwise *)
let check_rax (error : errors) : instruction list = 
  let endCheck = fresh_name "endErrorCheck" in
  AsmMov(ArgRegister R11, ArgRegister RAX) ::
  begin
    match error with 
    | One -> 
      AsmShl(ArgRegister RAX, ArgConstant (string_of_int 63))::
      AsmShr(ArgRegister RAX, ArgConstant (string_of_int 63))::
      AsmCmp(ArgRegister RAX, ArgConstant(string_of_int 0))::
      [AsmJe(endCheck)]
    | Two -> 
      AsmShl(ArgRegister RAX, ArgConstant (string_of_int 62))::
      AsmShr(ArgRegister RAX, ArgConstant (string_of_int 62))::
      AsmCmp(ArgRegister RAX, ArgConstant(string_of_int (3)))::
      AsmJe(endCheck):: []
    | Three ->   
      let isPointer = fresh_name "notPointer" in
      let endOfCheck = fresh_name "skipExtraCheck" in
      AsmMov(ArgRegister R8, ArgRegister RAX)::
      AsmXor(ArgRegister R8, ArgConstant(string_of_int(1)))::
      AsmShl(ArgRegister RAX, ArgConstant (string_of_int(62)))::
      AsmMov(ArgRegister R10, ArgConstant pointer_checker)::
      AsmXor(ArgRegister RAX, ArgRegister R10)::
      AsmMov(ArgRegister R10, ArgConstant(true_string))::
      AsmCmp(ArgRegister RAX, ArgRegister R10)::
      AsmJe(isPointer)::AsmJmp(endOfCheck)::
      AsmLabel(isPointer)::AsmMov(ArgRegister R10, ArgMemory(AddrByRegister(R8)))::
      AsmShr(ArgRegister R10, ArgConstant(string_of_int(63)))::
      AsmMov(ArgRegister R8, ArgConstant(string_of_int(0)))::
      AsmCmp(ArgRegister R10, ArgRegister R8)::
      AsmJe(endCheck)::AsmLabel(endOfCheck)::[]
    | Four -> 
      (* Note that RDX is used to pass the value we're checking against, 
         RAX has the tuple pointer *)
      AsmSub(ArgRegister RAX, ArgConstant(string_of_int(1)))::
      AsmMov(ArgRegister R8, ArgMemory(AddrByRegister(RAX)))::
      AsmCmp(ArgRegister R8, ArgRegister RDX)::
      AsmJg(endCheck)::
      [] 
    | Five -> 
      let isPointer = fresh_name "notPointer" in
      let endOfCheck = fresh_name "skipExtraCheck" in
      AsmMov(ArgRegister R8, ArgRegister RAX)::
      AsmXor(ArgRegister R8, ArgConstant(string_of_int(1)))::
      AsmShl(ArgRegister RAX, ArgConstant (string_of_int(62)))::
      AsmMov(ArgRegister R10, ArgConstant pointer_checker)::
      AsmXor(ArgRegister RAX, ArgRegister R10)::
      AsmMov(ArgRegister R10, ArgConstant(true_string))::
      AsmCmp(ArgRegister RAX, ArgRegister R10)::
      AsmJe(isPointer)::AsmJmp(endOfCheck)::
      AsmLabel(isPointer)::AsmMov(ArgRegister R10, ArgMemory(AddrByRegister(R8)))::
      AsmShr(ArgRegister R10, ArgConstant(string_of_int(63)))::
      AsmMov(ArgRegister R8, ArgConstant(string_of_int(1)))::
      AsmCmp(ArgRegister R10, ArgRegister R8)::
      AsmJe(endCheck)::AsmLabel(endOfCheck)::[]
  end
  @AsmMov(ArgRegister RDI, ArgConstant(code_of_errors error))::
   AsmPush(ArgRegister RAX)::
   AsmCall("stopWithError")::
   AsmLabel(endCheck)::
   AsmMov(ArgRegister RAX, ArgRegister R11)::[]
;;


(* A function that loops over the top of the stack and breaks down the parameters
   that were pushed before the call started. 
   Parameters: p -> expression list. Used as a counter pop as many times as we pushed
   Return: Instruction list containing a number of pops equal to the number of pushes.
          Each pop is placed in RAX and overwritten as we don't care about the value anymore *)
let rec pop_parameters (p : expr list) : instruction list = 
  match p with 
  | [] -> []
  | _::tail -> AsmPop(ArgRegister RAX)::pop_parameters tail
;;


(* Main function in this program. This is where the expression
   is interpreted into assembly.  *)
let rec compile_expression (env : environment) (e : expr)
  : instruction list =
  match e with 

  | EInt integer -> [AsmMov(ArgRegister RAX, ArgConstant (string_of_twice_int integer))]
  | EVar name -> [AsmMov(ArgRegister RAX, find_named_variable name env)]

  | EUnaryOp (op, e') -> 
    begin
      match op with
      | OpAfter -> compile_expression env e'@
                   check_rax One @
                   [AsmAdd(ArgRegister RAX, ArgConstant (string_of_int 2))]
      | OpBefore -> compile_expression env e'@
                    check_rax One @
                    [AsmSub(ArgRegister RAX, ArgConstant (string_of_int 2))]
      | OpIsInt -> compile_expression env e'@
                   [AsmShl(ArgRegister RAX, ArgConstant (string_of_int(63)));
                    AsmMov(ArgRegister R10, ArgConstant true_string);
                    AsmXor(ArgRegister RAX, ArgRegister R10)]
      | OpIsBool -> 
        let condTrue = fresh_name "skipFalse" in
        compile_expression env e'@
        [AsmShl(ArgRegister RAX, ArgConstant (string_of_int(62)));
         AsmMov(ArgRegister R10, ArgConstant bool_checker);
         AsmXor(ArgRegister RAX, ArgRegister R10);
         AsmMov(ArgRegister R10, ArgConstant true_string);
         AsmCmp(ArgRegister RAX, ArgRegister R10);
         AsmJe(condTrue); AsmMov(ArgRegister RAX, ArgConstant false_string);
         AsmLabel(condTrue)]
      | OpPrint -> compile_expression env e'@
                   AsmPush(ArgRegister RAX)::AsmMov(ArgRegister RDI, ArgRegister RAX)::
                   AsmCall("printValue")::AsmPop(ArgRegister RAX)::[]
      | OpIsTuple -> 
        let condTrue = fresh_name "skipFalse" in
        let endOfCheck = fresh_name "skipExtraCheck" in
        compile_expression env e'@
        AsmMov(ArgRegister R8, ArgRegister RAX)::
        AsmXor(ArgRegister R8, ArgConstant(string_of_int(1)))::
        AsmShl(ArgRegister RAX, ArgConstant (string_of_int(62)))::
        AsmMov(ArgRegister R10, ArgConstant pointer_checker)::
        AsmXor(ArgRegister RAX, ArgRegister R10)::
        AsmMov(ArgRegister R10, ArgConstant true_string)::
        AsmCmp(ArgRegister RAX, ArgRegister R10)::
        AsmJe(condTrue):: AsmMov(ArgRegister RAX, ArgConstant false_string)::
        AsmJmp(endOfCheck)::
        AsmLabel(condTrue)::AsmMov(ArgRegister R10, ArgMemory(AddrByRegister(R8)))::
        AsmShr(ArgRegister R10, ArgConstant(string_of_int(63)))::
        AsmMov(ArgRegister R8, ArgConstant(string_of_int(0)))::
        AsmCmp(ArgRegister R10, ArgRegister R8)::
        AsmJe(endOfCheck)::AsmMov(ArgRegister RAX, ArgConstant(false_string))::
        AsmLabel(endOfCheck)::[]
    end 

  | EBinaryOp (op,left,right) -> 
    let left' = compile_expression env left in 
    let (l_addr, env') = allocate_temp_variable env in 
    let store_left = AsmMov(l_addr, ArgRegister RAX) in 
    let right' = compile_expression env' right in 
    let (r_addr, _) = allocate_temp_variable env' in
    let store_right = AsmMov(r_addr, ArgRegister RAX) in 
    left'@
    begin
      match op with 
      | OpMinus
      | OpPlus
      | OpTimes
      | OpLessThan 
      | OpGreaterThan 
      | OpEqualTo ->  check_rax One
      | OpAnd 
      | OpOr -> check_rax Two
      | OpTupleIndex -> check_rax Three 
    end
    @store_left::
     right'@
    begin
      match op with 
      | OpMinus
      | OpPlus
      | OpTimes
      | OpLessThan 
      | OpGreaterThan 
      | OpEqualTo ->  check_rax One
      | OpAnd 
      | OpOr -> check_rax Two
      | OpTupleIndex -> check_rax One
    end
    @store_right::
     AsmMov(ArgRegister RAX, l_addr)::
     begin
       match op with 
       | OpMinus -> AsmSub(ArgRegister RAX, r_addr)::[]
       | OpPlus -> AsmAdd(ArgRegister RAX, r_addr)::[]
       | OpTimes -> AsmSar(ArgRegister RAX, ArgConstant (string_of_int 1))::
                    AsmIMul(ArgRegister RAX, r_addr)::[]
       | OpLessThan -> 
         let name = fresh_name "lessThan" in 
         let endCond = fresh_name "endCond" in
         AsmCmp(ArgRegister RAX, r_addr)::AsmJl(name)::
         AsmMov(ArgRegister RAX, ArgConstant(false_string))::
         AsmJmp(endCond)::
         AsmLabel(name)::
         AsmMov(ArgRegister RAX, ArgConstant(true_string))::
         AsmLabel(endCond)::[]
       | OpGreaterThan -> 
         let name = fresh_name "greaterThan" in 
         let endCond = fresh_name "endCond" in
         AsmCmp(ArgRegister RAX, r_addr)::AsmJg(name)::
         AsmMov(ArgRegister RAX, ArgConstant(false_string))::
         AsmJmp(endCond)::
         AsmLabel(name)::
         AsmMov(ArgRegister RAX, ArgConstant(true_string))::
         AsmLabel(endCond)::[]
       | OpEqualTo -> 
         let name = fresh_name "equal" in 
         let endCond = fresh_name "endCond" in
         AsmCmp(ArgRegister RAX, r_addr)::AsmJe(name)::
         AsmMov(ArgRegister RAX, ArgConstant(false_string))::
         AsmJmp(endCond)::
         AsmLabel(name)::
         AsmMov(ArgRegister RAX, ArgConstant(true_string))::
         AsmLabel(endCond)::[]
       | OpAnd -> AsmAnd(ArgRegister RAX, r_addr)::[]
       | OpOr -> [AsmOr(ArgRegister RAX, r_addr)]
       | OpTupleIndex -> 
         AsmMov(ArgRegister RDX, r_addr)::(* Using RDX to pass argument to this function *)
         AsmShr(ArgRegister RDX, ArgConstant(string_of_int(1)))::
         check_rax Four @
         AsmAdd(ArgRegister RDX, ArgConstant(string_of_int(2)))::
         AsmIMul(ArgRegister RDX, ArgConstant(string_of_int(8)))::
         AsmXor(ArgRegister RAX, ArgConstant(string_of_int(1)))::
         AsmMov(ArgRegister R10, ArgRegister RAX)::
         AsmAdd(ArgRegister R10, ArgRegister RDX)::
         AsmMov(ArgRegister RAX, ArgMemory(AddrByRegister(R10)))::
         []
     end

  | ELet (var,left,right) -> 
    let left' = compile_expression env left in 
    let (l_addr, _) = allocate_temp_variable env in
    let storeLeft = AsmMov(l_addr, ArgRegister RAX) in
    let env' = allocate_named_variable var env in
    let right' = compile_expression env' right in
    left'@storeLeft::right'
  | EIf (cond,e1,e2) -> 
    let e1block = fresh_name "e1block" in
    let endIf = fresh_name "endIf" in
    compile_expression env cond @
    check_rax Two @
    AsmMov(ArgRegister R10, ArgConstant true_string)::
    AsmCmp(ArgRegister RAX, ArgRegister R10)::
    AsmJe(e1block)::
    compile_expression env e2@
    AsmJmp(endIf)::
    AsmLabel(e1block)::
    compile_expression env e1@
    AsmLabel(endIf)::[]
  | EBool (bool) -> 
    begin
      match bool with 
      | true -> [AsmMov(ArgRegister RAX, ArgConstant true_string)]
      | false -> [AsmMov(ArgRegister RAX, ArgConstant false_string)]
    end 

  | EAppl (left, right, tail) -> (* TODO : Add tail call here *)
    let callFunc = fresh_name "callFunc" in
    let skipFunc = fresh_name "skipFunc" in
    let skipGC = fresh_name "skipGC" in
    let tailCall = fresh_name "tailCall" in
    compile_expression env left @
    let (left_addr, env') = allocate_temp_variable env in 
    AsmMov(left_addr, ArgRegister RAX):: 
    compile_expression env' right @
    let (right_addr, env'') = allocate_temp_variable env' in 
    AsmMov(right_addr, ArgRegister RAX)::
    AsmMov(ArgRegister RAX, left_addr)::
    check_rax Five @
    AsmXor(ArgRegister RAX, ArgConstant(string_of_int(1)))::
    AsmMov(ArgRegister R10, ArgMemory(AddrByRegister(RAX)))::
    AsmShl(ArgRegister R10, ArgConstant(string_of_int(1)))::
    AsmShr(ArgRegister R10, ArgConstant(string_of_int(1)))::
    AsmMov(ArgRegister R11, ArgMemory(AddrByRegisterOffset(RAX, 16))):: (* Modified 8 -> 16 bc GC word is in the middle *)
    AsmAdd(ArgRegister R10, ArgConstant(string_of_int(1)))::
    AsmCmp(ArgRegister R10, ArgRegister R11)::
    AsmJe(callFunc):: 
    (* a + 1 < p *)
    let (store_source, env''') = allocate_temp_variable env'' in 
    AsmMov(store_source, ArgRegister RAX)::
    AsmAdd(ArgRegister R10, ArgConstant(string_of_int(3))):: 
    let (storeArgCount, env'''' ) = allocate_temp_variable env''' in
    AsmMov(storeArgCount, ArgRegister R10)::
    AsmAdd(ArgRegister R10, ArgConstant(string_of_int(1)))::
    AsmIMul(ArgRegister R10, ArgConstant(string_of_int(8)))::
    AsmMov(ArgRegister RAX, ArgMemory(AddrByLabel("heap_cursor")))::
    AsmMov(ArgRegister RDX, ArgMemory(AddrByLabel("end_of_heap")))::
    AsmAdd(ArgRegister RAX, ArgRegister R10)::
    AsmCmp(ArgRegister RAX, ArgRegister RDX)::
    AsmJl(skipGC)::
    AsmJe(skipGC):: 
    AsmSub(ArgRegister RAX, ArgRegister RDX)::
    AsmPush(ArgRegister RCX)::
    AsmPush(ArgRegister RDX)::
    AsmPush(ArgRegister RSI)::
    AsmPush(ArgRegister RDI)::
    AsmPush(ArgRegister R8)::
    AsmPush(ArgRegister R9)::
    AsmPush(ArgRegister R10)::
    AsmPush(ArgRegister R11)::
    AsmMov(ArgRegister RDI, ArgRegister RAX):: (* Holds the size number of bytes we want to free *)
    AsmMov(ArgMemory(AddrByLabel("end_of_stack")), ArgRegister RSP)::
    AsmCall("gc")::
    AsmPop(ArgRegister R11)::
    AsmPop(ArgRegister R10)::
    AsmPop(ArgRegister R9)::
    AsmPop(ArgRegister R8)::
    AsmPop(ArgRegister RDI)::
    AsmPop(ArgRegister RSI)::
    AsmPop(ArgRegister RDX)::
    AsmPop(ArgRegister RCX)::
    AsmMov(ArgRegister RAX, ArgMemory(AddrByLabel("heap_cursor")))::
    AsmAdd(ArgRegister RAX, ArgRegister R10)::
    AsmLabel(skipGC)::
    AsmMov(ArgRegister RDI, ArgMemory(AddrByLabel("heap_cursor")))::
    AsmMov(ArgRegister R11, ArgRegister RDI)::
    AsmMov(ArgMemory(AddrByLabel("heap_cursor")), ArgRegister RAX)::
    AsmMov(ArgRegister RCX, storeArgCount)::
    AsmMov(ArgRegister RSI, store_source)::
    AsmRepMovsq:: 
    AsmMov(ArgRegister RAX, ArgMemory(AddrByRegister(R11)))::
    AsmAdd(ArgRegister RAX, ArgConstant(string_of_int(1)))::
    AsmMov(ArgMemory(AddrByRegister(R11)), ArgRegister RAX)::
    AsmMov(ArgRegister RAX, ArgRegister RDI)::
    AsmMov(ArgRegister R8, right_addr)::
    AsmMov(ArgMemory(AddrByRegister RAX), ArgRegister R8)::
    AsmMov(ArgRegister RAX, ArgRegister R11)::
    AsmAdd(ArgRegister RAX, ArgConstant(string_of_int(1)))::
    AsmJmp(skipFunc)::
    (* a + 1 = p *)
    AsmLabel(callFunc)::
    begin
      match tail with 
      | true -> 
        AsmMov(ArgRegister R11, ArgMemory(AddrByRegisterOffset(RBP, 16)))::
        AsmCmp(ArgRegister R10, ArgRegister R11):: (* Compares the number of params in the current function to what we're about to call *)
        AsmJl(tailCall)::
        AsmJe(tailCall)::[]
      | false -> []
    end
    @
    AsmAdd(ArgRegister RAX, ArgConstant(string_of_int(32)))::
    AsmSub(ArgRegister R10, ArgConstant(string_of_int(1)))::
    AsmMov(ArgRegister RCX, ArgRegister R10)::
    AsmMov(ArgRegister RSI, ArgRegister RAX)::
    AsmPush(ArgRegister RCX)::
    AsmPush(ArgRegister RDX)::
    AsmPush(ArgRegister RSI)::
    AsmPush(ArgRegister RDI)::
    AsmPush(ArgRegister R8)::
    AsmPush(ArgRegister R9)::
    AsmPush(ArgRegister R10)::
    AsmPush(ArgRegister R11)::
    AsmAdd(ArgRegister R10, ArgConstant(string_of_int(1)))::
    AsmIMul(ArgRegister R10, ArgConstant(string_of_int(8)))::
    AsmSub(ArgRegister RSP, ArgRegister R10)::
    let (parameterSize, _) = allocate_temp_variable env'''' in
    AsmMov(parameterSize, ArgRegister R10)::
    AsmMov(ArgRegister RDI, ArgRegister RSP)::
    AsmRepMovsq::
    AsmMov(ArgRegister R8, ArgRegister RSP)::
    AsmSub(ArgRegister R10, ArgConstant(string_of_int(8)))::
    AsmAdd(ArgRegister R8, ArgRegister R10)::
    AsmMov(ArgRegister R9, right_addr)::
    AsmMov(ArgMemory(AddrByRegister(R8)), ArgRegister R9)::
    AsmSub(ArgRegister RAX, ArgConstant(string_of_int(8)))::
    AsmMov(ArgRegister RCX, parameterSize)::
    AsmShr(ArgRegister RCX, ArgConstant(string_of_int(3)))::
    AsmPush(ArgRegister RCX)::(* Push the param count onto the stack *)
    AsmCall("[rax]")::
    AsmMov(ArgRegister R10, parameterSize)::
    AsmAdd(ArgRegister RSP, ArgRegister R10)::
    AsmPop(ArgRegister R11)::
    AsmPop(ArgRegister R10)::
    AsmPop(ArgRegister R9)::
    AsmPop(ArgRegister R8)::
    AsmPop(ArgRegister RDI)::
    AsmPop(ArgRegister RSI)::
    AsmPop(ArgRegister RDX)::
    AsmPop(ArgRegister RCX)::
    AsmJmp(skipFunc)::
    (* This is where we define the special case behavior if a function is a tail call *)
    AsmLabel(tailCall)::
    (* This block zeroes out the old parameters of the current call to be replaced as needed *)
    AsmMov(ArgRegister RDX, ArgRegister RAX)::
    AsmMov(ArgRegister RCX, ArgRegister R11)::
    AsmMov(ArgRegister R11, ArgConstant(string_of_int(3)))::
    AsmIMul(ArgRegister R11, ArgConstant(string_of_int(8)))::
    AsmAdd(ArgRegister R11, ArgRegister RBP)::
    AsmMov(ArgRegister RDI, ArgRegister R11)::
    AsmMov(ArgRegister RAX, ArgConstant(string_of_int(0)))::
    AsmRepStosq:: 
    AsmMov(ArgRegister RAX, ArgRegister RDX)::
    (* This block replaces old arguments with new ones *)
    AsmAdd(ArgRegister RAX, ArgConstant(string_of_int(32)))::(* Look at first datapoint in closure *)
    AsmSub(ArgRegister R10, ArgConstant(string_of_int(1)))::(* See how many things we actually have to move *)
    AsmMov(ArgRegister R11, ArgRegister R10)::
    AsmMov(ArgRegister RCX, ArgRegister R10)::
    AsmMov(ArgRegister RSI, ArgRegister RAX)::
    AsmMov(ArgRegister R10, ArgConstant(string_of_int(3))):: (* 3 = ret addr, param size, last arg *)
    AsmIMul(ArgRegister R10, ArgConstant(string_of_int(8)))::
    AsmAdd(ArgRegister R10, ArgRegister RBP)::(* Create pointer to RBP offset *)
    AsmMov(ArgRegister RDI, ArgRegister R10)::
    AsmRepMovsq::
    AsmMov(ArgRegister R9, right_addr)::
    AsmMov(ArgMemory(AddrByRegister(RDI)), ArgRegister R9):: (* This line might need to get fixed. Changed Offset + 24 to RDI *)
    AsmAdd(ArgRegister R11, ArgConstant(string_of_int(1)))::
    AsmMov(ArgMemory(AddrByRegisterOffset(RBP, 16)), ArgRegister R11)::
    AsmPop(ArgRegister R15)::
    AsmPop(ArgRegister R14)::
    AsmPop(ArgRegister R13)::
    AsmPop(ArgRegister R12)::
    AsmPop(ArgRegister RBX)::
    AsmMov(ArgRegister RSP, ArgRegister RBP)::
    AsmPop(ArgRegister RBP)::
    AsmSub(ArgRegister RAX, ArgConstant(string_of_int(8)))::
    AsmJmp("[rax]")::
    AsmLabel(skipFunc)::[]

  (* Definitng Tuples *)
  | ETuple argumentList -> 
    let skipGC = fresh_name "skipGC" in
    let tupSize = List.length argumentList in 
    AsmMov(ArgRegister RAX, ArgMemory(AddrByLabel("heap_cursor")))::
    (*     let (heapAddr, env') = allocate_temp_variable env in 
           AsmAdd(ArgRegister RAX, ArgConstant(string_of_int(1)))::
           AsmMov(heapAddr, ArgRegister RAX)::
           AsmSub(ArgRegister RAX, ArgConstant(string_of_int(1))):: *)
    AsmMov(ArgRegister RDX, ArgMemory(AddrByLabel("end_of_heap")))::
    AsmAdd(ArgRegister RAX, ArgConstant(string_of_int(((tupSize+2)*8))))::
    AsmCmp(ArgRegister RAX, ArgRegister RDX)::
    AsmJl(skipGC)::
    AsmJz(skipGC)::
    AsmSub(ArgRegister RAX, ArgRegister RDX):: 
    AsmMov(ArgRegister RDI, ArgRegister RAX):: (* Holds the size number of bytes we want to free *)
    AsmMov(ArgMemory(AddrByLabel("end_of_stack")), ArgRegister RSP)::
    AsmCall("gc")::
    AsmMov(ArgRegister RAX, ArgMemory(AddrByLabel("heap_cursor")))::
    AsmAdd(ArgRegister RAX, ArgConstant(string_of_int(((tupSize+2)*8))))::
    AsmLabel(skipGC)::
    let (heapAddr, env') = allocate_temp_variable env in 
    AsmMov(ArgRegister RBX, ArgMemory(AddrByLabel("heap_cursor")))::
    AsmAdd(ArgRegister RBX, ArgConstant(string_of_int(1)))::
    AsmMov(heapAddr, ArgRegister RBX)::
    AsmMov(ArgMemory(AddrByLabel("heap_cursor")), ArgRegister RAX)::
    AsmMov(ArgRegister RAX, heapAddr)::
    AsmSub(ArgRegister RAX, ArgConstant(string_of_int(1)))::
    AsmMov(ArgRegister R10, ArgConstant(string_of_int(tupSize)))::
    AsmMov(ArgMemory(AddrByRegister(RAX)), ArgRegister R10)::
    AsmMov(ArgRegister R9, ArgConstant (string_of_int(0))):: (* Added this and the following line to set GC word to 0 *)
    AsmMov(ArgMemory(AddrByRegisterOffset(RAX, 8)), ArgRegister R9)::
    let rec fill_tuple (arguments : expr list) (offset : int): instruction list = 
      match arguments with
      | [] -> []
      | head :: tail -> 
        compile_expression env' head @
        AsmMov(ArgRegister R13, heapAddr)::
        AsmSub(ArgRegister R13, ArgConstant(string_of_int(1)))::
        AsmMov(ArgMemory(AddrByRegisterOffset(R13, offset)), ArgRegister RAX)::
        fill_tuple tail (offset + 8)
    in 
    fill_tuple argumentList 16 @ (* Modified this line. Used to be 8. 16 allocates space for GC word*)
    AsmMov(ArgRegister RAX, heapAddr)::[] 
  | ESet (tuple,index,newVal) -> 
    compile_expression env tuple @
    check_rax Three @
    let (memTuple, env1) = allocate_temp_variable env in 
    AsmMov(memTuple, ArgRegister RAX)::
    compile_expression env1 index @
    check_rax One @
    AsmMov(ArgRegister RDX, ArgRegister RAX)::
    AsmShr(ArgRegister RDX, ArgConstant(string_of_int(1)))::
    AsmMov(ArgRegister RAX, memTuple)::
    check_rax Four @
    AsmAdd(ArgRegister RDX, ArgConstant(string_of_int(2))):: 
    AsmIMul(ArgRegister RDX, ArgConstant(string_of_int(8)))::
    AsmXor(ArgRegister RAX, ArgConstant(string_of_int(1)))::
    AsmAdd(ArgRegister RAX, ArgRegister RDX)::
    let (whereToChange, env2) = allocate_temp_variable env1 in 
    AsmMov(whereToChange, ArgRegister RAX)::
    compile_expression env2 newVal @
    AsmMov(ArgRegister R10, whereToChange)::
    AsmMov(ArgMemory(AddrByRegister(R10)), ArgRegister RAX)::[]
;;


let rec compile_declarations (env: environment)(dec : declaration) : instruction list = 
  match dec with  
  | DFunction (identifier,parameterList,expression) -> 
    let functionEnvironment = create_function_environment parameterList env 24 in (* Changed this to 24 to make room for paramSize *)
    let instructions = compile_expression functionEnvironment expression in 
    let rsp_offset = stack_memory_of_instruction_list instructions in 
    AsmLabel("fun_"^identifier)::
    AsmPush(ArgRegister RBP)::
    AsmMov(ArgRegister RBP, ArgRegister RSP)::
    AsmAdd(ArgRegister RSP, ArgConstant (string_of_int rsp_offset))::
    AsmPush(ArgRegister RBX)::
    AsmPush(ArgRegister R12)::
    AsmPush(ArgRegister R13)::
    AsmPush(ArgRegister R14)::
    AsmPush(ArgRegister R15)::
    instructions @ 
    AsmPop(ArgRegister R15)::
    AsmPop(ArgRegister R14)::
    AsmPop(ArgRegister R13)::
    AsmPop(ArgRegister R12)::
    AsmPop(ArgRegister RBX)::
    AsmSub(ArgRegister RSP, ArgConstant (string_of_int rsp_offset))::
    AsmMov(ArgRegister RSP, ArgRegister RBP)::
    AsmPop(ArgRegister RBP)::
    [AsmRet]
;;


let compile_program (d : declaration list) (e : expr)(env : environment) : instruction list =
  let declarations = List.concat (List.map (compile_declarations env) d) in
  let instructions = compile_expression env e in
  let rsp_offset = stack_memory_of_instruction_list instructions in 
  AsmPush(ArgRegister RBP)::
  AsmMov(ArgRegister RBP, ArgRegister RSP)::
  AsmAdd(ArgRegister RSP, ArgConstant (string_of_int rsp_offset))::
  AsmMov(ArgMemory(AddrByLabel("heap_cursor")), ArgRegister RDI)::
  AsmMov(ArgMemory(AddrByLabel("start_of_heap")), ArgRegister RDI)::
  AsmMov(ArgMemory(AddrByLabel("end_of_heap")), ArgRegister RSI)::
  AsmMov(ArgMemory(AddrByLabel("start_of_stack")), ArgRegister RBP)::
  AsmPush(ArgRegister RBX)::
  AsmPush(ArgRegister R12)::
  AsmPush(ArgRegister R13)::
  AsmPush(ArgRegister R14)::
  AsmPush(ArgRegister R15)::
  instructions @ 
  AsmPop(ArgRegister R15)::
  AsmPop(ArgRegister R14)::
  AsmPop(ArgRegister R13)::
  AsmPop(ArgRegister R12)::
  AsmPop(ArgRegister RBX)::
  AsmSub(ArgRegister RSP, ArgConstant (string_of_int rsp_offset))::
  AsmMov(ArgRegister RSP, ArgRegister RBP)::AsmPop(ArgRegister RBP)::
  [AsmRet] @ 
  declarations
;;

let create_data_section (decList : declaration list) : instruction list =
  AsmSection("data")::
  AsmAlign(8)::
  AsmLabel("global heap_cursor\nheap_cursor")::
  AsmDq(["0"])::
  AsmAlign(8)::
  AsmLabel("global start_of_stack\nstart_of_stack")::
  AsmDq(["0"])::
  AsmAlign(8)::
  AsmLabel("global end_of_stack\nend_of_stack")::
  AsmDq(["0"])::
  AsmAlign(8)::
  AsmLabel("global start_of_heap\nstart_of_heap")::
  AsmDq(["0"])::
  AsmAlign(8)::
  AsmLabel("global end_of_heap\nend_of_heap")::
  AsmDq(["0"])::
  let rec create_intial_closures (d : declaration list) : instruction list=
    match d with
    | [] -> [] 
    | head :: tail -> 
      begin 
        match head with 
        | DFunction (identifier,parameterList,_) -> 
          AsmAlign(8)::
          AsmLabel("closure_of_"^ identifier)::
          AsmDq(["0x8000000000000000"; "0"; string_of_int(List.length parameterList); 
                 "fun_"^identifier])::
          create_intial_closures tail 
      end
  in
  create_intial_closures decList 
;;

let rec create_base_environment (env : environment) (dec : declaration list): environment =
  match dec with 
  | [] -> env 
  | head :: tail ->
    begin 
      match head with 
      | DFunction(identifier, _, _) -> 
        let env' = store_func_location identifier env in 
        create_base_environment env' tail
    end 
;;

let compile_to_assembly_code (e : program) : string =
  let _ = check_well_formed e in
  let e' = identify_tail_calls e in
  match e' with
  | Program (funcList, expression) -> 
    let baseEnvironment = create_base_environment empty_environment funcList in
    let instructions = compile_program funcList expression baseEnvironment in
    let instruction_code = code_of_instruction_list instructions in
    let dataSection = code_of_instruction_list(create_data_section funcList) in

    dataSection ^
    "section .text\n" ^
    "extern printValue\n" ^
    "extern stopWithError\n" ^
    "extern gc\n" ^
    "global bird_main\n" ^
    "bird_main:\n" ^
    instruction_code ^
    "\n"
;;


(* 
Progress: 
Currently on the stack my arguments seem to have gotten reversed. 
gdb up to go up a stack frame. Break at stopwitherror. then break at the call. c twice
*)