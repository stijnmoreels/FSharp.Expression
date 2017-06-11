namespace FSharp.Expression

/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///
module Library = 
  
  /// Returns 42
  ///
  /// ## Parameters
  ///  - `num` - whatever
  let hello num = 42

module Parser =
    let evaluate expression = "True"

//    // Define an immutable stack
//  type ImmutableStack<'T> =
//    | Empty 
//    | Stack of 'T * ImmutableStack<'T>
//
//    member s.Push x = Stack(x, s)
//
//    member s.Pop() = 
//      match s with
//      | Empty -> failwith "Underflow"
//      | Stack(t, ts) -> 
//        (t, match ts with
//            | Empty -> Empty
//            | Stack(r, rs) -> Stack(r, rs))
//
//    member s.Top() = 
//      match s with
//      | Empty -> failwith "Contain no elements"
//      | Stack(_,st) -> st
//
//    member s.IEmpty = 
//      match s with
//      | Empty -> true
//      | _ -> false
//
//    member s.All() = 
//      let rec loop acc = function
//      | Empty -> acc
//      | Stack(t,st) -> loop (t::acc) st
//      loop [] s
//    
//    member s.Count() = s.All() |> List.length
