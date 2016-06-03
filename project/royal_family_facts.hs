royalfamily :: Program
royalfamily = [

(("mother",[Constant "emma",Constant "wilhelmina"]), []),
(("mother",[Constant "wilhelmina",Constant "juliana"]), []),
(("mother",[Constant "juliana",Constant "beatrix"]), []),
(("mother",[Constant "juliana",Constant "margriet"]), []),
(("mother",[Constant "juliana",Constant "irene"]), []),
(("mother",[Constant "juliana",Constant "christina"]), []),
(("mother",[Constant "margriet",Constant "maurits"]), []),
(("mother",[Constant "margriet",Constant "bernhard_jr"]), []),
(("mother",[Constant "margriet",Constant "pieter_christiaan"]), []),
(("mother",[Constant "margriet",Constant "floris"]), []),
(("mother",[Constant "beatrix",Constant "alexander"]), []),
(("mother",[Constant "beatrix",Constant "friso"]), []),
(("mother",[Constant "beatrix",Constant "constantijn"]), []),
(("mother",[Constant "maxima",Constant "amalia"]), []),
(("mother",[Constant "maxima",Constant "alexia"]), []),
(("mother",[Constant "maxima",Constant "ariane"]), []),

(("husband",[Constant "bernhard",Constant "juliana"]), []),
(("husband",[Constant "claus",Constant "beatrix"]), []),
(("husband",[Constant "pieter",Constant "margriet"]), []),
(("husband",[Constant "alexander",Constant "maxima"]), []),
(("husband",[Constant "friso",Constant "mabel"]), []),
(("husband",[Constant "constantijn",Constant "laurentien"]), []),


(("female",[Constant "irene"]), []),
(("female",[Constant "christina"]), []),
(("female",[Constant "amalia"]), []),
(("female",[Constant "alexia"]), []),
(("female",[Constant "ariane"]), []),
(("female",[Variable "X"]), [("mother", [Variable "X",Variable "_"])]),
(("female",[Variable "X"]), [("husband",[Variable "_",Variable "X"])]),

(("male",[Constant "maurits"]), []),
(("male",[Constant "bernhard_jr"]), []),
(("male",[Constant "pieter_christiaan"]), []),
(("male",[Constant "floris"]), []),
(("male",[Variable "X"]), [("husband", [Variable "X", Variable "_"])]),

(("father", [Variable "X", Variable "Y"]), [("husband", [Variable "X", Variable "Z"]), ("mother", [Variable "Z", Variable "Y"])]),
(("child", [Variable "X", Variable "Y"]), [("father", [Variable "Y", Variable "X"])]),
(("child", [Variable "X", Variable "Y"]), [("mother", [Variable "Y", Variable "X"])]),

(("grandfather", [Variable "X", Variable "Y"]), [("child", [Variable "Y", Variable "Z"]), ("father", [Variable "X", Variable "Z"])])

]
