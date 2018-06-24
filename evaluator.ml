(*
	UNIVERSIDADE FEDERAL DO RIO GRANDE DO SUL
	INSTITUTO DE INFORMÁTICA
	DEPARTAMENTO DE INFORMÁTICA TEÓRICA
	INF05516 - Semântica Formal
	Prof. Dr. Alvaro Moreira

    Camila Primieri - 172662
    Pietra Freitas - 242285

    --------------------------------------------

    Avaliador da linguagem L1 com listas e exceções de acordo com a semântica operacional Big Step de L1

*)

type variable = string

type operator = Sum | Diff | Mult | Div | Eq | And | Or | Not

type tipo  = TyX | TyInt | TyBool | TyFn of tipo * tipo | TyList of tipo


type expr = Num of int 
          | Bool of bool 
          | Bop of operator * expr * expr
          | If of expr * expr * expr 
          | Var of variable 
          | App of expr * expr 
          | Lam of variable * tipo * expr 
          | Let of variable * tipo * expr * expr
          | Lrec of variable * tipo * tipo * variable * tipo * expr * expr
          | Nil of nil
          | List of list
          | Isempty of expr
          | Hd of expr
          | Tl of expr
          | Raise of exn
          | Try of expr * expr


type value = Vnum of int 
           | Vbool of bool 
           | Vclos of variable * expr * env
           | Vrclos of variable * variable * expr * env
and  
     env = (variable * value) list

