
signature JSON = sig
  datatype json = N  
                | S of string 
                | I of int 
                | R of real 
                | B of bool
                | A of json list 
                | O of (string * json) list

  val toString: json -> string
  val parse: string -> json 
  val objectGet: json -> string -> json option
  val isObject: json -> bool 

  val sGet: json -> string
  val iGet: json -> int 

end

structure Json :> JSON = struct

  datatype json = N  
                | S of string 
                | I of int 
                | R of real 
                | B of bool
                | A of json list 
                | O of (string * json) list

  fun pairToString (str, data) = (
    concat ["\"", str, "\": ", toString data]
  )
  and toString data = case data 
    of N => "null"
     | S str => "\"" ^ str ^ "\""
     | I i => Int.toString i 
     | R r => Real.toString r 
     | B b => Bool.toString b
     | A xs => (let
                 val innerStrs = map toString xs 
                 val contentStr = String.concatWith ", " innerStrs
               in
                  "[" ^ contentStr ^ "]"
               end)
     | O ps => (let
                  val innerStrs = map pairToString ps 
                  val contentStr = String.concatWith ",\n  " innerStrs
                in
                  "{\n  " ^ contentStr ^ "\n}"
                end)


  structure JsonParser = JSONParser(struct
    type json_data = json  

    fun json_object l = case l 
      of [] =>  O []  
       | (O ps)::[] => O ps 
       | (O ps)::(O ps')::rest => (
         json_object ((O (ps @ ps'))::rest)
       )
       | _ => raise Match 

    fun json_pair (s, d) = O [(s, d)]
    fun json_array l = A l
    fun json_value v = v
    fun json_string s = S s
    fun json_int i = I i
    fun json_real r = R r
    fun json_bool b = B b
    fun json_null () = N

    fun error_handle (msg,pos,data) =
          raise Fail ("Error: " ^ msg ^ " near " ^ Int.toString pos ^ " data: " ^ data)
  end)

  val parse = JsonParser.parse

  fun objectGet json str = case json
    of O [] => NONE 
     | O ((str', v) :: ps) => (
       if str =  str'
       then SOME v
       else objectGet (O ps) str
     ) 
     | _ => raise Match

  fun isObject json = case json
    of O _ => true 
     | _ => false 

  fun sGet json = case json
    of S s => s 
     | _ => raise Match 

  fun iGet json = case json
    of I i => i 
     | _ => raise Match 
    






end

