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

type variable = string


type operator = Sum | Diff | Mult | Div | Eq | And | Or | Not


type tipo  = TyVar of string | TyInt | TyBool | TyFn of tipo * tipo | TyList of tipo


type expr = Num of int 
          | Bool of bool 
          | Bop of operator * expr * expr
          | If of expr * expr * expr 
          | Var of variable 
          | App of expr * expr 
          | Lam of variable * tipo * expr 
          | Let of variable * tipo * expr * expr
          | Lrec of variable * tipo * tipo * variable * tipo * expr * expr
          | Nil
          | List of expr * expr
          | Isempty of expr
          | Hd of expr
          | Tl of expr
          | Raise
          | Try of expr * expr


type value = Vnum of int 
           | Vbool of bool 
           | Vclos of variable * expr * env
           | Vrclos of variable * variable * expr * env
and  
     env = (variable * value) list


(*
  AMBIENTE
*)

(* Inclui variáveis novas no ambiente *)
let updateEnv variable tipo environment : env = match environment with
  |[] -> [(variable, tipo)]
  | hd::tl -> List.append [(variable, tipo)] environment

(* Procura uma variável específica no ambiente. Se não achar, retorna Raise *)
let rec searchEnv variable environment : tipo = match environment with
  | [] -> Raise
  | (k, v)::tl ->
    if (k = variable)
    then v
    else searchEnv variable tl

let emptyEnv : env = []


(* 
  FUNÇÃO COLLECTTYEQS
*)


(* 
  FUNÇÃO UNIFY
*)


(* 
  FUNÇÃO APPLYSUBS
*)


(* 
  ALGORITMO TYPEINFER
*)

(* Recebe o ambiente de tipos e o programa para ser testado *)
typeinfer(typeEnv, program) =

    let
        (* A função collectTyEqs retorna um tipo (ou variável de tipo) e um conjunto de equações de tipo *)
        (ty, typeEqSet) = collectTyEqs(typeEnv, program)

        (* A função unify retorna um substituição sigma, que é um mapeamento de variáveis de tipo para tipos 
           Pode falhar caso o conjunto não tenha solução, porque o programa é mal tipado *)
        sigma = unify(typeEqSet)
    in
        (* A função applySubs aplica a essa substituição ao tipo retornado por collectTyEqs e retorna o tipo final da expressão *)
        applySubs(sigma, ty)
end
