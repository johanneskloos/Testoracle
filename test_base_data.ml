(** Basic data for tests. *)
open Types

(** Common instances of JavaScript values - base instances. *)
let vundef = OUndefiend
let vnull = ONull
let vtrue = OBoolean true
let vfalse = OBoolean false
let v0 = ONumberInt 0
let v1 = ONumberInt 1
let v2 = ONumberInt 2
let vpi = ONumberFloat 3.14
let vsqrt2 = ONumberInt 1.41
let valpha = OString "alpha"
let vbeta = OString "beta"
let vmain = OSymbol "main"

(** Three versions of function tables, two equivalent but permuted, and one different. *)
let funcin1 = Local { from_toString = "func 1 toString"; from_jalangi = Some "func 1 jalangi" }
let funcin2 = Local { from_toString = "func 2 toString"; from_jalangi = Some "func 2 jalangi" }
let funcin3 = Local { from_toString = "func 3 toString"; from_jalangi = Some "func 3 jalangi" }
let funcstd = Local { from_toString = "func std toString"; from_jalangi = None }
let funcext1 = External 1
let funcext2 = External 2

let functab1 = [| funcin1; funcin2; funcstd; funcext1 |]
(** functab2 is a cyclic permutation of functab1 *)
let functab2 = [| funcin2; funcstd; funcext1; funcin1 |]
(** functab3 is distinct from functab1 and functab2 *)
let functab3 = [| funcin1; funcin3; funcstd; funcext2 |]

(** Three versions of object tables, two equivalent but permuted, and one different. *)
