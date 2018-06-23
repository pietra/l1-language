(*
	UNIVERSIDADE FEDERAL DO RIO GRANDE DO SUL
	INSTITUTO DE INFORMÁTICA
	DEPARTAMENTO DE INFORMÁTICA TEÓRICA
	INF05516 - Semântica Formal
	Prof. Dr. Alvaro Moreira

    Camila Primieri - 
    Pietra Freitas - 242285

    --------------------------------------------

    Avaliador da linguagem L1 com listas e exceções de acordo com a semântica operacional Big Step de L1

*)

(* TODO: inserir listas *)

type variable = string;;

type operator =
	Sum
	| Sub
	| Mult
	| Div
	| Equal
	| And
	| Or
	| Not
;;

type tipo =
	TyInt
	| TyBool
	| TyFn of tipo * tipo
	| TyLst(* TODO: list *)
;;

type expr =
	Num of int
	| Bool of bool
	| Bop of expr * operator * expr
	| If of expr * expr * expr
	| Var of variable
	| App of expr * expr
	| Func of variable * tipo * expr
	| Let of variable * tipo * expr * expr
	| LetRec of variable * (tipo * tipo) * (variable * tipo * expr) * expr
	| Head of variable (* TODO: list *)
	| Tail of variable (* TODO: list *)
;;

type value =
	Vnum of int
	| Vbool of bool
	| Vclos of variable * expr * env
	| Vrclos of variable * variable * expr * env
	(* TODO: list *)
and
	env = (variable * value) list
;;
