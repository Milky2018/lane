# Lane

## Stages

parse : Text -> RProg 
trans : RProg -> MTProg 
init : MTProg -> LVProg 
tc : LVProg -> LProg 
eval : LProg -> LVal 

## TODO List 

* Algebraic data types
* Generic types 
* IO