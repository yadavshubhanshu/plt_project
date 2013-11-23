module NameMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)

let env = NameMap.empty;;
let env = NameMap.add "c" 2 env;;
let ent = NameMap.add "d" 25 env;;
print_int (NameMap.find "c" env);;
print_float (NameMap.find "d" ent);;
