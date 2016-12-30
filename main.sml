val _ = RunCML.doit (fn () =>
  (let

    val protocol = "CSCI-652-calendar"
    val discoveryPortList = [5000, 5001, 5002, 5003, 5004]
    val anySockAddrList = map INetSock.any discoveryPortList
    val hostName = NetHostDB.getHostName ()
    val entryOp = NetHostDB.getByName hostName
    val localAddr = NetHostDB.addr (valOf entryOp)
    val localAddrStr = NetHostDB.toString localAddr

    val serverSource = Client.discoverServers (protocol, anySockAddrList, localAddrStr)

    val ui = UI.run ();
    val username = UI.getUsername ui
    val serverPort = UI.getServerPort ui 

    val _ = Server.announceLocalServer (protocol, anySockAddrList, localAddrStr, serverPort, username)
    val clientSource = Server.listenForClients serverPort 
    val store = Storage.create (serverSource, clientSource, User.Info {username=username, addr=localAddrStr, port=serverPort})
    val _ = UI.setStore (ui, store)

  in
    ()
  end),
  NONE
);
