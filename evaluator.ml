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
        let (t1, c1) = collectTyEqs environment e1 in
        let (t2, c2) = collectTyEqs environment e2 in
        (match op with

            Sum -> (TyInt, c1 @ c2 @ [(t1, TyInt); (t2, TyInt)])
          | Sub -> (TyInt, c1 @ c2 @ [(t1, TyInt); (t2, TyInt)])
          | Mult -> (TyInt, c1 @ c2 @ [(t1, TyInt); (t2, TyInt)])
          | Div -> (TyInt, c1 @ c2 @ [(t1, TyInt); (t2, TyInt)])
          | Eq -> (TyBool, c1 @ c2 @ [(t1, TyInt); (t2, TyInt)])
          | And -> (TyBool, c1 @ c2 @ [(t1, TyBool); (t2, TyBool)])
          | Or -> (TyBool, c1 @ c2 @ [(t1, TyBool); (t2, TyBool)])
        )

    (* Not *)
    | Not(e) ->
        let (t, c) = collectTyEqs environment e in
        (TyBool, c @ [(t, TyBool)])

    (* Condicional *)
    | If(e1, e2, e3) ->
        let (t1, c1) = collectTyEqs environment e1 in
        let (t2, c2) = collectTyEqs environment e2 in
        let (t3, c3) = collectTyEqs environment e3 in
        (t2, c1 @ c2 @ c3 @ [(t1, TyBool); (t2, t3)])

    (* Variável *)
    | Var(variable) ->
        let t = searchEnv variable environment in
        (t, [])

    (* Aplicação *)
    | App(e1, e2) ->
        let (t1, c1) = collectTyEqs environment e1 in
        let (t2, c2) = collectTyEqs environment e2 in
        (TyX("App"), c1 @ c2 @ [(t1, TyFn(t2, TyX("New")))])

    (* Função *)
    | Func(variable, t, e) ->
        let (t1, c1) = collectTyEqs (updateEnv variable t environment) e in
        (TyFn(t, t1), c1)

    (* Let *)
    | Let(variable, t, e1, e2) ->
        let (t1, c1) = collectTyEqs environment e1 in
        let (t2, c2) = collectTyEqs (updateEnv variable t environment) e2 in
        (t2, c1 @ c2 @ [(t, t1)])

    (* Let Rec *)
    | Lrec(f, t1, t2, variable, t3, e1, e2) ->
        let t4 = TyFn(t1, t2) in
        let update1 = updateEnv variable t3 environment in
        let update2 = updateEnv f t4 update1 in
        let (t5, c5) = collectTyEqs update2 e1 in
        let (t6, c6) = collectTyEqs (updateEnv f t4 environment) e2 in
        (t6, c5 @ c6 @ [(t5, t2)])

    (* Nil *)
    | Nil ->
        (TyList(TyX("Nil")), [])

    (* List *)
    | List(e1, e2) ->
        let (t1, c1) = collectTyEqs environment e1 in
        let (t2, c2) = collectTyEqs environment e2 in
        (t2, c1 @ c2 @ [(TyList(t1), t2)])

    (* IsEmpty*)
    | Isempty(e) ->
        let (t, c) = collectTyEqs environment e in
        (TyBool, c @ [(t, TyList(TyX("IsEmpty")))])

    (* Hd *)
    | Hd(e) ->
        let (t, c) = collectTyEqs environment e in
        (TyX("Hd"), c @ [(t, TyList(TyX("Hd")))])

    (* Tl *)
    | Tl(e) ->
        let (t, c) = collectTyEqs environment e in
        (TyList(TyX("Tl")), c @ [(t, TyList(TyX("Tl"))])

    (* Raise *)
    | Raise ->
        (TyList(TyX("Raise")), [])

    (* Try *)
    | Try(e1, e2) ->
        let (t1, c1) = collectTyEqs environment e1 in
        let (t2, c2) = collectTyEqs environment e2 in
        (t2, c1 @ c2 @ [(t1, t2)])
;;

(*
  FUNÇÃO UNIFY: dadas as equações de tipo, verifica se é possível uma substituição

    Regras:
    1. σ, (int = int) :: C −→ σ, C
    2. σ, (bool = bool) :: C −→ σ, C
    3. σ, (X = X) :: C −→ σ, C
    4. σ, (X = T) :: C −→ σ @ [(X, T)], {T/X} C se X não ocorre em T
    5. σ, (T = X) :: C −→ σ @ [(X, T)], {T/X} C se X não ocorre em T
    6. σ, (T1 → T2 = T3 → T4) :: C −→ σ, (T1 = T3) :: (T2 = T4) :: C
    7. σ, (T1 list = T2 list) :: C −→ σ, (T1 = T2) :: C
*)

exception UnifyError of string

let unify tyEquations =

    (* Tipo X ocorre em T? *)
    let rec occurs tyX tyT = match tyT with
        | TyList(tyT1) -> occurs tyX tyT1
        | TyFn(tyT1,tyT2) -> occurs tyX tyT1 || occurs tyX tyT2
        | (TyInt | TyBool ) -> false
        | TyX(s) -> (s=tyX)
    in

    (* Substitui X por T no tipo S*)
    let rec subsType tyX tyT tyS = match tyS with
        | TyList(tyS1) -> TyList(subsType tyX tyT tyS1)
        | TyFn(tyS1, tyS2) -> TyFn(subsType tyX tyT tyS1, subsType tyX tyT tyS2)
        | TyInt -> TyInt
        | TyBool -> TyBool
        | TyX(s) -> (if s=tyX then tyT else TyX(s)) (* se for X, troca por T*)
    in

    (* Para cada equação do conjunto, substitui tyX por tyT *)
    let subsEquations tyX tyT tyEquations =
        List.map (fun (tyS1,tyS2) -> (subsType tyX tyT tyS1, subsType tyX tyT tyS2)) tyEquations
    in

    let rec unify_rec tyEquations = match tyEquations with
        | [] -> []

        | (TyInt,TyInt) :: tail -> unify_rec tail (* 1 *)

        | (TyBool,TyBool) :: tail -> unify_rec tail (* 2 *)

        | (TyX(tyX),tyT) :: tail -> (* 4 *)
            if tyT = TyX(tyX) then unify_rec tail (* 3 *)
            else if occurs tyX tyT then raise (UnifyError "Unify failed. X occurs in T.")
            else List.append (unify_rec (subsEquations tyX tyT tail)) [(TyX(tyX),tyT)]

        | (tyT,TyX(tyX)) :: tail -> (* 5 *)
            if tyT = TyX(tyX) then unify_rec tail (* 3 *)
            else if occurs tyX tyT then raise (UnifyError "Unify failed. X occurs in T.")
            else List.append (unify_rec (subsEquations tyX tyT tail)) [(TyX(tyX),tyT)]

        | (TyFn(tyT1,tyT2),TyFn(tyT3,tyT4)) :: tail -> unify_rec ((tyT1,tyT3) :: (tyT2,tyT4) :: tail) (* 6 *)

        | (TyList(tyT1),TyList(tyT2)) :: tail -> unify_rec ((tyT1,tyT2) :: tail) (* 7 *)

        | (tyS,tyT)::tail -> raise (UnifyError "Unify failed. No solutions found.")

     in unify_rec tyEquations

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
