(* $Id$ *)

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

let rec catelim_idclassstring a1 a2 =
  match a1, a2 with
    NoClass      , tail -> "NoClass" :: tail
  | FormulaClass , tail -> "FormulaClass" :: tail
  | VariableClass, tail -> "VariableClass" :: tail
  | ConstantClass, tail -> "ConstantClass" :: tail
  | NumberClass  , tail -> "NumberClass" :: tail
  | StringClass  , tail -> "StringClass" :: tail
  | OperatorClass, tail -> "OperatorClass" :: tail
  | SubstClass   , tail -> "SubstClass" :: tail
  | BagClass c   , tail -> "BagClass(" :: catelim_idclassstring c (")" :: tail)
  | ListClass c  , tail -> "ListClass(" :: catelim_idclassstring c (")" :: tail)

let idclassstring = Listfuns.catelim2stringfn catelim_idclassstring
