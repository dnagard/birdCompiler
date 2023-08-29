open Batteries;;
open HatchLanguage;;

open Printf;;

open Asts;;

exception IllFormed of string list;;

type funcInfo = int Map.String.t;;


(* Creates a map of all the function names as they are in scope in the main expression. 
   Takes a string map and a declaration list and returns a string list with all the funciton 
   names stored as variables so they pass the "unbound variable detection" *)
let rec create_base_map (m : Set.String.t) (dec : declaration list): Set.String.t =
  match dec with 
  | [] -> m 
  | head :: tail ->
    begin 
      match head with 
      | DFunction(identifier, _, _) -> 
        let map = Set.String.add identifier m in 
        create_base_map map tail
    end 
;;



let rec find_duplicate_params (name : string) (s : string list) (m :  Set.String.t): string list =
  match s with 
  | head::tail ->
    begin
      match Set.String.find_opt head m with 
      | Some var -> begin ["Function " ^ name ^ " declares a duplicate parameter " ^ var ^ "."] end
      | None -> 
        let m' = Set.String.add head m in
        find_duplicate_params name tail m'
    end
  | [] -> []
;;

let rec find_duplicates_in_decl (d : declaration list)(m : Set.String.t) : string list = 
  match d with 
  | d'::tail ->
    begin 
      match d' with
      | DFunction(identifier, parameters, _) -> 
        (find_duplicate_params identifier parameters Set.String.empty) @
        begin 
          match Set.String.find_opt identifier m with 
          | Some _ -> ["Duplicate definition of function " ^ identifier ^"."] @ find_duplicates_in_decl tail m
          | None -> 
            let m' = Set.String.add identifier m in
            find_duplicates_in_decl tail m'
        end
    end
  | [] -> []
;;

let rec find_unbound_vars_in_dec (d : declaration list)(baseMap : Set.String.t) : string list =
  match d with 
  | [] -> []
  | dec::tail ->
    begin 
      match dec with 
      | DFunction(_, parameters, expression) -> 
        let rec populate_map (m : Set.String.t) (s : string list) : Set.String.t = 
          match s with 
          | [] -> m 
          | param::nextParam -> populate_map (Set.String.add param m) nextParam
        in 
        let map = populate_map (baseMap) parameters in
        let rec recurse_expression (m : Set.String.t) (e : expr) : string list = 
          match e with
          | EInt _
          | EBool _ -> []
          | EVar var -> 
            begin 
              match Set.String.find_opt var m with 
              | Some _ -> []
              | None -> ["Unbound variable " ^ var ^"."]
            end
          | EUnaryOp (_, e') -> recurse_expression m e'
          | EBinaryOp (_, e1, e2) -> recurse_expression m e1 @ recurse_expression m e2
          | ELet(name, e1, e2) ->
            begin 
              match Set.String.find_opt name m with 
              | Some _ -> begin 
                  recurse_expression m e1 @
                  recurse_expression m e2 end
              | None ->  
                let m' = Set.String.add name m in 
                recurse_expression m e1 @ recurse_expression m' e2
            end
          | ESet (e1, e2, e3)
          | EIf (e1, e2, e3) ->
            recurse_expression m e1@
            recurse_expression m e2 @
            recurse_expression m e3
          | EAppl (e1, e2, _) -> recurse_expression m e1 @ recurse_expression m e2 (* TODO: Add tail call here *)
          | ETuple arguments -> List.concat(List.map (recurse_expression m) arguments)

        in 
        recurse_expression map expression @
        find_unbound_vars_in_dec tail baseMap
    end 
;;

let rec find_unbound_vars_in_expr (m : Set.String.t) (e : expr)  : string list = 
  match e with
  | EInt _
  | EBool _ -> []
  | EVar var -> 
    begin 
      match Set.String.find_opt var m with 
      | Some _ -> []
      | None -> ["Unbound variable " ^ var ^"."]
    end
  | EUnaryOp (_, e') -> find_unbound_vars_in_expr m e'
  | EBinaryOp (_, e1, e2) -> find_unbound_vars_in_expr m e1 @ find_unbound_vars_in_expr m e2
  | ELet(name, e1, e2) ->
    begin 
      match Set.String.find_opt name m with 
      | Some _ ->
        find_unbound_vars_in_expr m e1 @
        find_unbound_vars_in_expr m e2 
      | None ->  
        let m' = Set.String.add name m in 
        find_unbound_vars_in_expr m e1 @ find_unbound_vars_in_expr m' e2
    end
  | ESet (e1, e2, e3)
  | EIf (e1, e2, e3) ->
    find_unbound_vars_in_expr m e1@
    find_unbound_vars_in_expr m e2 @
    find_unbound_vars_in_expr m e3
  | EAppl(e1, e2, _ ) -> find_unbound_vars_in_expr m e1 @ find_unbound_vars_in_expr m e2 (* TODO: Add tail call here *)
  | ETuple arguments -> List.concat(List.map (find_unbound_vars_in_expr m) arguments)
;;


(* This is the old arity check for Eagle. Not necessary in Falcon as functions are variables 
   bound to closures. *)

(* let rec recurse_expression_for_unbound_funcs (m : int Map.String.t) (e : expr) : string list = 
   match e with
   | EInt _
   | EBool _ 
   | EVar _ -> []
   | EUnaryOp (_, e1) -> recurse_expression_for_unbound_funcs m e1
   | EBinaryOp (_, e1, e2) 
   | ELet(_, e1, e2) -> recurse_expression_for_unbound_funcs m e1 @ recurse_expression_for_unbound_funcs m e2
   | EIf (e1, e2, e3) ->
    recurse_expression_for_unbound_funcs m e1@
    recurse_expression_for_unbound_funcs m e2 @
    recurse_expression_for_unbound_funcs m e3
   | ECall (functionName, arguments) -> 
    begin 
      match Map.String.find_opt functionName m with 
      | None -> ["Function " ^ functionName ^ " is not defined."] @
      let rec recurse_arguments (m : int Map.String.t) (a1 : expr list ) : string list = 
        begin
          match a1 with 
          | [] -> []
          | head::tail -> 
            recurse_expression_for_unbound_funcs m head @ recurse_arguments m tail
        end
      in 
      recurse_arguments m arguments 

      | Some v -> 
        begin
          if ((List.length arguments) == v) then 
            []
          else 
            ["Function " ^ functionName ^ " is called with an incorrect number of arguments."]
        end 

        @

        let rec recurse_arguments (m : int Map.String.t) (a1 : expr list ) : string list = 
          begin
            match a1 with 
            | [] -> []
            | head::tail -> 
              recurse_expression_for_unbound_funcs m head @ recurse_arguments m tail
          end
        in 
        recurse_arguments m arguments 
    end
    |ETuple arguments ->         
      let rec recurse_arguments (m : int Map.String.t) (a1 : expr list ) : string list = 
      begin
        match a1 with 
        | [] -> []
        | head::tail -> 
          recurse_expression_for_unbound_funcs m head @ recurse_arguments m tail
      end
    in 
    recurse_arguments m arguments
   ;; *)

(* let rec find_func_problems (d: declaration list) (funcMap : int Map.String.t) (main : expr) : string list =
   (* This internal recursive function populates the map with funciton names and 
     the number of parameters they require for later reference *)
   let rec populateMap (funcNamesAndParams : int Map.String.t) (decList : declaration list) : int Map.String.t =
    match decList with 
    | [] -> funcNamesAndParams
    | head::tail -> 
      begin
        match head with 
        |DFunction(funcName, funcParam, _) -> 
          populateMap (Map.String.add funcName (if List.is_empty funcParam then 0 else List.length funcParam) funcNamesAndParams) tail 
      end 
   in 
   let m = populateMap funcMap d in 

   (* This is the beginning of the finding func problems part *)
   let rec inside_declarations (currentDec : declaration list) : string list = 
    match currentDec with 
    | [] -> []
    | dec::next -> 
      begin
        match dec with 
        |DFunction(_, _, expression) ->
          recurse_expression_for_unbound_funcs m expression @ inside_declarations next 
      end
   in 
   inside_declarations d @ recurse_expression_for_unbound_funcs m main

   ;; *)


(* This function produces a list of compile-time errors found in a program. *)
let check_program_for_errors (p : program) : string list =
  match p with
  | Program(declarations, mainExpr) ->
    let base_map = create_base_map Set.String.empty declarations in 
    find_duplicates_in_decl declarations Set.String.empty @
    find_unbound_vars_in_dec declarations base_map@
    find_unbound_vars_in_expr base_map mainExpr  
    (*  find_func_problems declarations Map.String.empty mainExpr *)
;;

(* This function will check a program for compile-time errors.  If any errors
   are found, an IllFormed exception is thrown.  Otherwise, unit is returned. *)
let check_well_formed (p : program) : unit =
  let errors = check_program_for_errors p in
  if List.is_empty errors then () else raise (IllFormed errors);
;;


(* Adds the tail call marker to our AST *) 
let rec mark_tail_in_expr (b : bool)(e : expr) : expr = 
  match e with 
  | EInt _ 
  | EVar _
  | EBool _ -> e
  | EUnaryOp(op,expression) -> EUnaryOp(op, mark_tail_in_expr false expression)
  | EBinaryOp(binary_operator, expr1, expr2) -> EBinaryOp(binary_operator, mark_tail_in_expr false expr1, mark_tail_in_expr false expr2)
  | ETuple expr_list -> ETuple(List.map (mark_tail_in_expr false) expr_list)
  | ELet(string, expr1, expr2) -> ELet(string, mark_tail_in_expr false expr1, mark_tail_in_expr true expr2)
  | EIf(expr1, expr2, expr3) -> EIf(mark_tail_in_expr false expr1, mark_tail_in_expr true expr2, mark_tail_in_expr true expr3)
  | EAppl(closure, arg, _) -> EAppl(mark_tail_in_expr false closure, mark_tail_in_expr false arg, b)
  | ESet(closure, index, expr) -> ESet(mark_tail_in_expr false closure, mark_tail_in_expr false index, mark_tail_in_expr false expr)
;;

let identify_tail_calls (p : program) : program = 
  match p with 
  | Program(declarations, mainExpr) -> 
    let rec iterateDeclarations (d : declaration list) : declaration list =
      match d with 
      | [] -> []
      | head :: tail -> 
        begin
          match head with 
          | DFunction(string, stringList, expression) -> DFunction(string, stringList, mark_tail_in_expr true expression) :: iterateDeclarations tail
        end
    in 
    Program(iterateDeclarations declarations, mainExpr)
;;
