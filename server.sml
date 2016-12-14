structure Server = struct 

  fun announceLocalServer (protocol, anySockAddrList, localAddrStr, serverPort, userName) = (
  let
    val sock = INetSock.UDP.socket ()
    val _ = Socket.Ctl.setBROADCAST (sock, true)
    val data = [  
      ("protocol", Json.S protocol),
      ("addr", Json.S localAddrStr),
      ("port", Json.I serverPort),
      ("name", Json.S userName)
    ]

    fun loop () = (let  
      val dataStr = Json.toString(Json.O (
        (
          "time", 
          Json.S (
            Time.toString (Time.now ())
          )
        )::data
      ))
      val dataByteVec = Byte.stringToBytes dataStr
      val dataSlice = Word8VectorSlice.slice(dataByteVec, 0, NONE)
      val _ = map (fn anySockAddr => Socket.sendVecTo (sock, anySockAddr, dataSlice)) anySockAddrList
      val _ = CML.sync (CML.timeOutEvt (Time.fromSeconds 5))
    in
      loop()
    end)
  in 
    CML.spawn loop 
  end)

  fun listenForClients port = (let

    val ch = CML.channel ()

    val sock = INetSock.TCP.socket()
    val _ = Socket.Ctl.setREUSEADDR(sock, true)
    val _ = (Socket.bind(sock, INetSock.any port)) handle x => (print ("Server port " ^ (Int.toString port) ^ " is not available\n"); raise x)
    val _ = Socket.listen(sock, 9)

    fun loop () = (let
      val _ = CML.send (ch, Socket.accept sock)
    in
      loop ()
    end)

    val _ = CML.spawn loop
  in
    ch
  end)

end


