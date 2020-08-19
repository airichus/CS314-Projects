open Proj2_types;;

let getStartSymbol (g : grammar) : string = match g with
  |(head, tail) -> head (*just return the head*)
;;

let getNonterminals (g : grammar) : string list = match g with
  |(head, tail) -> let rec temp l acc = match l with
                    |[] -> acc
                    |(e1, e2)::rest -> let temp2 = acc@[e1] (*append*)
                                  in temp rest temp2
                    in temp tail [] (*call for the rest of the grammar*)
;;

let getInitFirstSets (g : grammar) : symbolMap = match g with
  |(head, tail) -> let rec temp l tempmap = match l with (*helper function to go through*)
    |[] -> tempmap (*just return the map if it ends up being empty*)
    |(e1, e2)::rest -> let temp2 = SMap.add e1 SymbolSet.empty tempmap (*get the LHS of a tuple and add the empty map*)
                        in temp rest temp2
    (*(did not work: SMap.singleton a (SymbolSet.empty))*)
                    in temp tail SMap.empty (*call for the rest of the grammar*)
;;

let getInitFollowSets (g : grammar) : symbolMap = match g with
|(head, tail) -> let rec helper l tempmap = match l with
   |[] -> tempmap (*just return the map if its empty*)
   |(e1, e2)::rest -> if head <> e1 (*ocaml doesnt use !=, need to use <>. if the head is not equal to x, then add*)
                     then 
                      let temp = SMap.add e1 SymbolSet.empty tempmap
                        in helper rest temp
                else (*otherwise, add the eof symbol*)
                     let temp = SMap.add e1 (SymbolSet.singleton "eof") tempmap
                         in helper rest temp
              in helper tail SMap.empty (*call for the rest of the grammar*)
;;

let rec computeFirstSet (first : symbolMap) (symbolSeq : string list) : SymbolSet.t =
  let rec temp symbol acc = (*helper function thatll use pattern matching to identify the terminal vs non-terminal conditions*)
    match symbol with
      |[] -> SymbolSet.add "eps" acc (*if its empty, just throw it into an empty/epsilon*)
      |(head::tail) -> if SMap.mem head first (*if the head and tail exist, check if the head is located in the symbolMap*)
                        then
                          if SymbolSet.mem "eps" (SymbolSet.union (SMap.find head first) acc) (*Check to see if epsilon exists in the union of the value with the current final set*)
                            then (*If true, then remove the epsilon and union the two together -> need to do remove on the outside*)
                              temp tail (SymbolSet.remove "eps" (SymbolSet.union (SMap.find head first) acc))
                          else (*otherwise union the two together*)
                            SymbolSet.union (SMap.find head first) acc
                        else (*otherwise just return the head -> needs singleton*)
                          SymbolSet.singleton head
  in  (*the in statement for the helper function that uses an accumulator to retrieve the final set*)
    let acc = SymbolSet.empty
      in temp symbolSeq acc (*the in statement which calls the helper function with the initial symbolSequence and accumulator*)
;;
(*look at charlie's Q&A for info*)
let recurseFirstSets (g : grammar) (first : symbolMap) firstFunc : symbolMap = (*the main method provided*)
  let rec recurseFirstSetsHelper first firstFunc rule_list = match rule_list with (*helper method that gets the first set*)
    |[] -> first (*if its empty, just return the symbolMap*)
    |(head::tail) -> match head with (*if there are in fact values, match and split into lhs and rhs to call the union of the functions*)
                      |(lhs, rhs) -> let temp = SMap.add lhs (SymbolSet.union(SMap.find lhs first)(firstFunc first rhs)) first
                                      in recurseFirstSetsHelper temp firstFunc tail
  in
    let rule_list = (snd g)
      in recurseFirstSetsHelper first firstFunc rule_list
;;

let rec getFirstSets (g : grammar) (first : symbolMap) firstFunc : symbolMap =
  if (SMap.equal (SymbolSet.equal) (recurseFirstSets g first computeFirstSet) first)
    then
     first (*just return the symbol map once complete*)
  else (*otherwise keep calling the function recursively to update the map*)
    getFirstSets g (recurseFirstSets g first computeFirstSet) (computeFirstSet)
;;

let rec updateFollowSet (first : symbolMap) (follow : symbolMap) (nt : string) (symbolSeq : string list) : symbolMap = match symbolSeq with
  |[] -> follow (*if its an empty set then just output the follow set as is *)
  |(head::tail) -> if SMap.mem head first (*check to make sure the head is inside the first symbolMap*)
                    then 
                      if SymbolSet.mem "eps" (computeFirstSet first tail) (*if epsilon is inside the first set, make sure to remove it when unioning*)
                        then
                          let temp = (SMap.add head (SymbolSet.union (SymbolSet.union (SymbolSet.remove "eps" (computeFirstSet first tail)) (SMap.find head follow)) (SMap.find nt follow)) follow)
                            in
                              updateFollowSet first temp nt tail
                      else  (*otherwise add without having to remove epsilon from the set*)
                        let temp = (SMap.add head (SymbolSet.union (SMap.find head follow) (computeFirstSet first tail)) follow)
                          in
                            updateFollowSet first temp nt tail
                    else (*if the head isnt inside the first symbolMap, just call again*)
                      updateFollowSet first follow nt tail
;;
(*same thing as recurseFirstSets basically*)
let recurseFollowSets (g : grammar) (first : symbolMap) (follow : symbolMap) followFunc : symbolMap =
  let rec recurseFollowSetsHelper follow followFunc rule_list = match rule_list with 
    |[] -> follow
    |(e1::rest) -> match e1 with (*if theres values then do the match splitting with the first element and combine*)
                    |(lhs, rhs) -> let temp = (followFunc first follow lhs rhs)
                                    in recurseFollowSetsHelper temp followFunc rest
  in
    let rule_list = (snd g)
      in recurseFollowSetsHelper follow followFunc rule_list
;;
(*same thing as getFirstSets basically*)
let rec getFollowSets (g : grammar) (first : symbolMap) (follow : symbolMap) followFunc : symbolMap =
  if (SMap.equal (SymbolSet.equal) (recurseFollowSets g first follow updateFollowSet) follow)
    then
     follow (*just return the symbol map once complete*)
    else (*otherwise keep calling the function recursively to update the map with a new value for follow*)
      getFollowSets g first (recurseFollowSets g first follow updateFollowSet) (updateFollowSet)
;;

let rec getPredictSets (g : grammar) (first : symbolMap) (follow : symbolMap) firstFunc : ((string * string list) * SymbolSet.t) list =
  let rec getPredictHelper rule_list first follow firstFunc acc = match rule_list with
    |[] -> acc (*if empty, return the accumulator*)
    |(head::tail) -> match head with 
                      |(lhs,rhs) -> if SymbolSet.mem "eps" (firstFunc first rhs) (*if epsilon is found, need to subtract it*)
                                      then (*get the first set minus epsilon and union it with the follow all attached to the accumulator and use it as the acc parameter*)
                                        getPredictHelper tail first follow firstFunc ((head, (SymbolSet.union (SymbolSet.remove "eps" (firstFunc first rhs)) (SMap.find lhs follow)))::acc)
                                    else (*otherwise dont*)
                                      getPredictHelper tail first follow firstFunc((head, (firstFunc first rhs))::acc)
    in
      List.rev(getPredictHelper (snd g) first follow firstFunc []) (*need to reverse it like they talked about on sakai forums*)
;;

let tryDerive (g : grammar) (inputStr : string list) : bool =
  (*
      let first = getFirstSets g (getInitFirstSets g) computeFirstSet in (*use first in follow*)
        let follow = getFollowSets g first _____ updateFollowSet in (*use follow in predict*)
          let predict = getPredictSets g first follow computeFirstSet in (*use predict for tryDeriveHelper*)
            let tryDeriveHelper predict sentence input : bool = match sentence with (*do the stuff from the Q&A + lecture5/7*)
              |[] ->

              in 
  (*the code above is what i was able to convert from charlie's Q&A session before stopping*)
  *)
false;;

let tryDeriveTree (g : grammar) (inputStr : string list) : parseTree =
  (* YOUR CODE GOES HERE *)
Terminal "empty";;

let genParser g = tryDerive g;;
let genTreeParser g = tryDeriveTree g;;
