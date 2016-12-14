structure User = struct

  datatype info = Info of {addr: string, port: int, username: string}

  fun toString (Info {addr, port, username}) = (
    String.concat [
      username, "@", addr, ":", (Int.toString port)
    ]
  )

  fun fromString str = (let
    fun parse () = (let
      val (username, suffix) = case String.tokens (fn c => c = #"@") str
        of username::suffix::[] => (username, suffix)
         | _ => raise Domain 
      val (addr, portStr) = case String.tokens (fn c => c = #":") suffix 
        of addr::portStr::[] => (addr, portStr)
         | _ => raise Domain 
      val port = valOf (Int.fromString portStr) 
    in
      Info {addr=addr, port=port, username=username}
    end)
  in
    (SOME (parse ())) handle _ => NONE
  end)

end
