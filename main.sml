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

    (* 
     * TO DO: (all in storage.sml)
     *
     * handle AddMeeting request by connecting to servers (including self if
     * self (shortId 0)is specified) using serverList 
     *
     * modify addClient to create request handler for client and add its
     * handle to clientList
     *
     * handle newtork requests from clients for CheckAvailability.    
     *
     * handle newtork requests from servers for Cancellation or Confirmation.    
     *
     * avoid inconsistency by preventing peers from concurrently responding to more than
     * one client request or to make requests while responding to a client.    
     *
     * have each client request lock from every server it needs and wait until all
     * respond.
     *
     * have server reply to lock request if it's not busy, or place request
     * in queue if it has given lock to another.
     *
     * have client send release request when done making calendar.
     *
     * avoid deadlock by mandating that locks be acquired in alphabetical order
     * of username@ip:port.    
     *)

  in
    ()
  end),
  NONE
);
