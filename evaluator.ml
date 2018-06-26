(*
  UNIVERSIDADE FEDERAL DO RIO GRANDE DO SUL
  INSTITUTO DE INFORMÁTICA
  DEPARTAMENTO DE INFORMÁTICA TEÓRICA
  INF05516 - Semântica Formal
  Prof. Dr. Alvaro Moreira

    Camila Primieri - 172662
    Pietra Freitas - 242285

    --------------------------------------------

    Avaliador da linguagem L1 com listas e exceções de acordo com a semântica operacional de L1
*)


(*
  LINGUAGEM L1
*)

type variable = string;;


type operator = Sum | Sub | Mult | Div | Eq | And | Or
;;

type tipo = TyX of string
            | TyInt 
            | TyBool 
            | TyFn of tipo * tipo 
            | TyList of tipo
and 
     typeEnv = (variable * tipo) list
;;


type expr = Num of int 
          | Bool of bool 
          | Bop of operator * expr * expr
          | Not of expr
          | If of expr * expr * expr 
          | Var of variable 
          | App of expr * expr 
          | Func of variable * tipo * expr 
          | Let of variable * tipo * expr * expr
          | Lrec of variable * tipo * tipo * variable * tipo * expr * expr
          | Nil
          | List of expr * expr
          | Isempty of expr
          | Hd of expr
          | Tl of expr
          | Raise
          | Try of expr * expr
;;


type value = Vnum of int 
           | Vbool of bool 
           | Vclos of variable * expr * env
           | Vrclos of variable * variable * expr * env
and  
     env = (variable * value) list
;;


(*
  AMBIENTE
*)

(* Inclui variáveis novas no ambiente de tipos *)
let updateEnv variable tipo environment : typeEnv = match environment with
  |[] -> [(variable, tipo)]
  | hd::tl -> List.append [(variable, tipo)] environment
;;

(* Procura uma variável específica no ambiente. Se não achar, retorna o tipo de Raise *)
let rec searchEnv variable environment : tipo = match environment with
  | [] -> TyX("variable not found")
  | (k, v)::tl ->
    if (k = variable)
    then v
    else searchEnv variable tl
;;

(* Ambiente de tipos vazio *)
let emptyEnv : typeEnv = []
;;


(* 
  FUNÇÃO COLLECTTYEQS
*)

let rec collectTyEqs (environment:typeEnv) (e:expr) = 
    match e with

    (* VALORES *)
      Num(e) -> (TyInt, [], [])
    | Bool(e) -> (TyBool, [], [])

    (* OPERAÇÕES BINÁRIAS *)
    | Bop(op, e1, e2) ->
        let (exp1, aux1, aux2) = collectTyEqs environment e1 in
        let (exp2, aux1, aux2) = collectTyEqs environment e2 in
        (match op, exp1, exp2 with

            Sum, TyInt, TyInt -> (TyInt, [e1; e2], [exp1; exp2])
          | Sub, TyInt, TyInt -> (TyInt, [e1; e2], [exp1; exp2])
          | Mult, TyInt, TyInt -> (TyInt, [e1; e2], [exp1; exp2])
          | Div, TyInt, TyInt -> (TyInt, [e1; e2], [exp1; exp2])
          | Eq, TyInt, TyInt -> (TyBool, [e1; e2], [exp1; exp2])
          | And, TyBool, TyBool -> (TyBool, [e1; e2], [exp1; exp2])
          | Or, TyBool, TyBool -> (TyBool, [e1; e2], [exp1; exp2])
          | _ -> (TyX("BOP not found"), [], [])
        )

    (* NOT *)
    | Not(e) -> 
        let (exp1, aux1, aux2) = collectTyEqs environment e in
        (match exp1 with 
            TyBool -> (TyBool, [e], [exp1])
          | _ -> (TyX("NOT not found"), [], [])
        )

    (* CONDICIONAL *)
    | If(e1, e2, e3) ->
        let (exp1, aux1, aux2) = collectTyEqs environment e1 in
        if exp1 = TyBool
        then 
          let (exp2, aux1, aux2) = collectTyEqs environment e2 in
          let (exp3, aux1, aux2) = collectTyEqs environment e3 in
          ( if exp2 = exp3
            then (exp2, [e1; e2; e3], [exp1; exp2; exp3])
            else (TyX("IF error"), [], [])
          ) 
        else (TyX("IF not found"), [], [])

    (* VARIÁVEL *)
    | Var(variable) -> 
        let var = searchEnv variable environment in
        (var, [], [])

    (* APLICAÇÃO *)
    | App(e1, e2) ->
        let (exp1, aux1, aux2) = collectTyEqs environment e1 in
        let (exp2, aux1, aux2) = collectTyEqs environment e2 in
        (match exp1 with
            TyFn(t1, t2) -> (if t2 = exp2
                              then (exp2, [e1; e2], [TyFn(exp1, exp2); TyFn(exp1, exp2)])
                              else (TyX("APP not found"), [], []))
          | _ -> (TyX("APP not found"), [], [])
        )

    (* FUNÇÃO *)
    | Func(variable, t, e) -> 
        let (exp1, aux1, aux2) = collectTyEqs (updateEnv variable t environment) e in
        (TyFn(t, exp1), [e], [])

    (* ERRO *)
    | _ -> (TyX("Expression not found"), [], [])
;;

(* 
  FUNÇÃO UNIFY
*)

let unify typeEqSet = TyInt

;;

(* 
  FUNÇÃO APPLYSUBS
*)

let applySubs sigma ty = TyBool

;;


(* 
  ALGORITMO TYPEINFER
*)

(* Recebe o ambiente de tipos e o programa para ser testado *)
let typeInfer (environment:typeEnv) (program:expr) : tipo =

  (* A função collectTyEqs retorna um tipo (ou variável de tipo) e um conjunto de equações de tipo *)
  let (ty, typeEqSet, typeConstraintsSet) = collectTyEqs environment program in

    (* A função unify retorna um substituição sigma, que é um mapeamento de variáveis de tipo para tipos 
       Pode falhar caso o conjunto não tenha solução, porque o programa é mal tipado *)
    let sigma = unify typeEqSet in

    (* A função applySubs aplica a essa substituição ao tipo retornado por collectTyEqs e retorna o tipo final da expressão *)
    applySubs sigma ty

;;

(*
  TESTES
*)

(* Limpa o ambiente *)
let environment = emptyEnv;;

let not_test = Not(Bool(true));;

typeInfer environment not_test;;