(* $Id$ *)

(* SubstClass is for substitutions that are hard to understand 
 * - see function idclass in term.sml 
 *)

type idclass = NoClass
             | FormulaClass
             | VariableClass
             | ConstantClass
             | NumberClass
             | StringClass
             | OperatorClass
             | SubstClass
             | BagClass of idclass
             | ListClass of idclass

val catelim_idclassstring : idclass -> string list -> string list
val idclassstring : idclass -> string
