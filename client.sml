structure Client = struct

  fun discoverServers (protocol, anySockAddrList, localAddrStr) = (let

    val ch = CML.channel ()

    fun bind ss = (let
      val sock = INetSock.UDP.socket ()
    in case ss 
      of anySockAddr :: ss' =>
        ((Socket.bind (sock, anySockAddr); (sock, anySockAddr)) handle SysErr => (
          Socket.close sock; bind ss'
        ))
       | _ => (Socket.close sock; print "client binding error\n"; raise Bind)
    end)

    val (sock, anySockAddr) = bind anySockAddrList

    fun loop () = (let 
      val (vec, _) = (Socket.recvVecFrom (sock, 1024)) handle SysErr => (
        print ("client receive error\n"); (Byte.stringToBytes(""), anySockAddr)
      )

      val dataStr = Byte.bytesToString vec 
      val dataJson = Json.parse dataStr
      val getByKey = Json.objectGet dataJson

      val foreignAddr = Json.sGet (valOf (getByKey "addr"))

      val connectionPort = Json.iGet (valOf (getByKey "port"))
      val foreignProtocol = Json.sGet (valOf (getByKey "protocol"))
      val time = Json.sGet (valOf (getByKey "time"))
      val foreignName = Json.sGet (valOf (getByKey "name"))

      val _ = (
        if (protocol = foreignProtocol)
        then (
          CML.send (ch, User.Info {
            addr=foreignAddr, 
            port=connectionPort, 
            username=foreignName
          })
        )
        else ()
      )

      val _ = CML.sync (CML.timeOutEvt (Time.fromSeconds 5))
    in
      loop () 
    end) 
    val _  = CML.spawn loop;
  in
    ch
  end)

  fun connectToServer (addr, port) = (let
    val sa = INetSock.toAddr (addr, port)
    val sock = INetSock.TCP.socket ()
    fun call () = (let
      val _ = Socket.connect (sock, sa)
      val msgIVar = Util.recvVecAsync (sock, 1024)
    in
      (sock, msgIVar)
    end) handle x => (Socket.close sock; raise x)
  in
    call () 
  end)



  fun sendMessage () = ()

end
