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

    (* Valores *)
      Num(e) -> (TyInt, [])
    | Bool(e) -> (TyBool, [])

    (* Operações Binárias *)
    | Bop(op, e1, e2) ->
        let (exp1, aux) = collectTyEqs environment e1 in
        let (exp2, aux) = collectTyEqs environment e2 in
        (match op with

            Sum -> (TyInt, [exp1; exp2; TyInt; TyInt])
          | Sub -> (TyInt, [exp1; exp2; TyInt; TyInt])
          | Mult -> (TyInt, [exp1; exp2; TyInt; TyInt])
          | Div -> (TyInt, [exp1; exp2; TyInt; TyInt])
          | Eq -> (TyBool, [exp1; exp2; TyInt; TyInt])
          | And -> (TyBool, [exp1; exp2; TyBool; TyBool])
          | Or -> (TyBool, [exp1; exp2; TyBool; TyBool])
        )

    (* Not *)
    | Not(e) -> 
        let (exp1, aux) = collectTyEqs environment e in
        (TyBool, [exp1; TyBool])

    (* Condicional *)
    | If(e1, e2, e3) ->
        let (exp1, aux) = collectTyEqs environment e1 in
        let (exp2, aux) = collectTyEqs environment e2 in
        let (exp3, aux) = collectTyEqs environment e3 in
        (exp2, [exp1; exp2; exp3; TyBool; exp2])

    (* Variável *)
    | Var(variable) -> 
        let var = searchEnv variable environment in
        (var, [])

    (* Aplicação *)
    | App(e1, e2) ->
        let (exp1, aux) = collectTyEqs environment e1 in
        let (exp2, aux) = collectTyEqs environment e2 in
        (TyX("New"), [exp1; exp2; TyFn(exp1, TyX("New"))])

    (* Função *)
    | Func(variable, t, e) -> 
        let (exp1, aux) = collectTyEqs (updateEnv variable t environment) e in
        (TyFn(t, exp1), [exp1])

    (* Let *)
    | Let(variable, t, e1, e2) ->
        let (exp1, aux) = collectTyEqs environment e1 in
        let (exp2, aux) = collectTyEqs (updateEnv variable t environment) e2 in
        (exp2, [exp1; exp2; t])

    (* Let Rec *)
    | Lrec(f, t1, t2, variable, t3, e1, e2) ->
        let t4 = TyFn(t1, t2) in
        let update1 = updateEnv variable t3 environment in
        let update2 = updateEnv f t4 update1 in
        let (exp1, aux) = collectTyEqs update2 e1 in 
        let (exp2, aux) = collectTyEqs (updateEnv f t4 environment) e2 in
        (exp2, [exp1; exp2; t2])

    (* Erro *)
    | _ -> (TyX("Expression not found"), [])
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
  let (ty, typeEqSet) = collectTyEqs environment program in

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