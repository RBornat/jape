(* $Id$ *)

module type T =
  sig
    (* SubstClass is for substitutions that are hard to understand 
     * - see fun idclass in term.sml 
     *)
    type idclass =
        NoClass
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
  end
(* $Id$ *)

module M
  (AAA :
    sig
      val catelim2stringfn :
        ('a -> string list -> string list) -> 'a -> string
    end)
  :
  T =
  struct
    open AAA
    type idclass =
        NoClass
      | FormulaClass
      | VariableClass
      | ConstantClass
      | NumberClass
      | StringClass
      | OperatorClass
      | SubstClass
      | BagClass of idclass
      | ListClass of idclass
    let rec catelim_idclassstring a1 a2 =
      match a1, a2 with
        NoClass, tail -> "NoClass" :: tail
      | FormulaClass, tail -> "FormulaClass" :: tail
      | VariableClass, tail -> "VariableClass" :: tail
      | ConstantClass, tail -> "ConstantClass" :: tail
      | NumberClass, tail -> "NumberClass" :: tail
      | StringClass, tail -> "StringClass" :: tail
      | OperatorClass, tail -> "OperatorClass" :: tail
      | SubstClass, tail -> "SubstClass" :: tail
      | BagClass c, tail ->
          "BagClass(" :: catelim_idclassstring c (")" :: tail)
      | ListClass c, tail ->
          "ListClass(" :: catelim_idclassstring c (")" :: tail)
    let idclassstring = catelim2stringfn catelim_idclassstring
  end
