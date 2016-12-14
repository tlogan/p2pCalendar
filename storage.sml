structure Storage = struct

  datatype request = GetServerInfoList 
                   | SetServerInfoList of (User.info * int) list
                   | GetCalendar
                   | AddMeeting of Calendar.timeslot * User.info list
                   | GetUser 
                   | LockForUser of User.info * (bool SyncVar.ivar)
                   | ReleaseForUser of User.info * (bool SyncVar.ivar)

  datatype response = ServerInfoList of (User.info * int) list 
                    | Calendar of Calendar.calendar 
                    | User of User.info

  datatype storage = Storage of {requestChan: request CML.chan, responseChan: response CML.chan}

  fun refreshList serverList = (let
    fun condition (User.Info {addr, port, username}, shortId) = (let
      fun connect () = (let
        val (sock, msgIVar) = Client.connectToServer (valOf (NetHostDB.fromString addr), port)
        val msgOp = CML.select [ 
          CML.wrap(CML.timeOutEvt (Time.fromSeconds 1), fn () => NONE),
          CML.wrap(SyncVar.iGetEvt msgIVar, fn msg => SOME (Byte.bytesToString msg))
        ]
        val _ = Socket.close sock
      in
        case msgOp
          of SOME msg => msg = username 
           | NONE => false
      end)
    in
      (connect ()) handle x => false
    end)
  in
    List.filter condition serverList 
  end)

  fun setServerInfoList (store, newList) = (let
    val Storage {requestChan = reqCh, ...} = store
    val _ = CML.send (reqCh, SetServerInfoList newList)
  in
    ()
  end)


  fun getUser store = (let
    val Storage {requestChan = reqCh, responseChan = resCh} = store
    val _  = CML.send (reqCh, GetUser)
    val user = case CML.recv resCh 
      of User user => user 
       | _ => raise (Fail "should never happen") 
  in
    user
  end)

  fun lockForUser (store, user) = (let
    val resIVar = SyncVar.iVar () 
    val Storage {requestChan = reqCh,... } = store
    val _  = CML.send (reqCh, LockForUser (user, resIVar))
    val locked = SyncVar.iGet resIVar 
  in
    locked 
  end)

  fun releaseForUser (store, user) = (let
    val resIVar = SyncVar.iVar () 
    val Storage {requestChan = reqCh,... } = store
    val _  = CML.send (reqCh, ReleaseForUser (user, resIVar))
    val released = SyncVar.iGet resIVar 
  in
    released 
  end)

  fun addMeeting(store, slot, users) = (let
    val Storage {requestChan = reqCh, responseChan = resCh} = store
    val _ = CML.send (reqCh, AddMeeting (slot, users))
  in
    ()
  end)


  fun getServerInfoList store = (let
    val Storage {requestChan = reqCh, responseChan = resCh} = store
    val _  = CML.send (reqCh, GetServerInfoList)
    val refreshedList = (case CML.recv resCh 
      of ServerInfoList servList => (refreshList servList)
       | _ => raise Domain)
    val _ = setServerInfoList (store, refreshedList)
  in
   refreshedList 
  end)

  fun getCalendar store = (let
    val Storage {requestChan = reqCh, responseChan = resCh} = store
    val _ = CML.send (reqCh, GetCalendar)
    val calendar = case CML.recv resCh
      of Calendar cal => cal
       | _ => raise Domain
  in
    calendar
  end)


  fun requestMeeting (store, slot, shortIds) = (let
    val Storage {requestChan = reqCh, ...} = store

    val user = getUser store 
    val serverList = getServerInfoList store

    val valid = (let
      fun existsCondition shortId (_, serverShortId) = (
        shortId = serverShortId 
      )
      fun allCondition shortId = (
        List.exists (existsCondition shortId) serverList
      )
    in
      List.all allCondition shortIds 
    end)

    val allUsers = map (fn p => #1 p) serverList
    val users = (let
      fun fil (userInfo, sid) = List.exists (fn shortId => sid = shortId) shortIds
      val us = map (fn (userInfo, _) => userInfo) (List.filter fil serverList)
    in
      us
    end)
  in
    if valid
    then (let
      val msgr = ClientMsgr.create users 
      val locked = ClientMsgr.requireLocks msgr user (* blocks until all locks acquired or error *)
      val available = (locked andalso ClientMsgr.allAvailable msgr slot)
      val _ = (
        if available
        then ClientMsgr.scheduleMeeting msgr (slot, users)
        else false 
      )
      val released = (ClientMsgr.releaseLocks msgr user)
    in
      ()
    end)
    else ()
  end)


  datatype 'af incoming = Request of request 
                    | ServerInfo of User.info 
                    | Client of ('af, Socket.active Socket.stream) Socket.sock * 'af Socket.sock_addr 


  fun create (serverSource, clientSource, user) = (let


    val requestChan = CML.channel ()
    val responseChan = CML.channel ()

    val store = Storage {requestChan=requestChan, responseChan=responseChan}

    fun loop (serverCount, serverList, clientList, calendar, userWithLock, lockQueue) = (let

      fun continue () = loop (serverCount, serverList, clientList, calendar, userWithLock, lockQueue)

      fun respondWithServerList () = (let 
        val _ = CML.send (responseChan, ServerInfoList serverList)
      in
        continue ()
      end)

      fun respondWithCalendar () = (let 
        val _ = CML.send (responseChan, Calendar calendar)
      in
        continue ()
      end)

      fun respondWithUser () = (let
        val _ = CML.send (responseChan, User user)
      in
        continue ()
      end)

      fun lockOrQueue (user, resIVar) = (case userWithLock
        of NONE => (
            SyncVar.iPut (resIVar, true); 
            loop (serverCount, serverList, clientList, calendar, SOME user, lockQueue)
          )
         | SOME _ => loop (serverCount, serverList, clientList, calendar, userWithLock, lockQueue @ [(user, resIVar)])
      )

      fun release (user, resIVar) = (let
         val (newUserWithLock, newLockQueue) = (case userWithLock
          of NONE => (userWithLock, lockQueue) 
           | SOME u => (
             if u = user andalso (List.length lockQueue) = 0 
             then (NONE, lockQueue) 
             else if u = user 
             then (let
               val (lockingUser, lockingIVar) = List.hd lockQueue 
               val _ = SyncVar.iPut (lockingIVar, true); 
             in
               (SOME lockingUser, List.tl lockQueue)
             end)
             else (userWithLock, lockQueue)
           )
         )
         val _ = SyncVar.iPut (resIVar, true); 
      in
        loop (serverCount, serverList, clientList, calendar, newUserWithLock, List.filter (fn (userInfo, _) => user <> userInfo) newLockQueue)
      end)


      fun respond req = case req 
        of GetServerInfoList => respondWithServerList ()
         | SetServerInfoList newList => loop (serverCount, newList, clientList, calendar, userWithLock, lockQueue)
         | GetCalendar => respondWithCalendar () 
         | AddMeeting (slot, users) => loop (serverCount, serverList, clientList, Calendar.fillTimeslot(calendar, slot, users), userWithLock, lockQueue) 
         | GetUser => respondWithUser ()
         | LockForUser (user, resIVar) => lockOrQueue (user, resIVar)
         | ReleaseForUser (user, resIVar) => release (user, resIVar)

      val msg = CML.select [
        CML.wrap(CML.recvEvt serverSource, fn si => ServerInfo si),
        CML.wrap(CML.recvEvt clientSource, fn c => Client c),
        CML.wrap(CML.recvEvt requestChan, fn r => Request r) 
      ]

      fun addServerInfo si = (let
        val (User.Info {addr, port, username}) = si
        fun f (User.Info {addr=a, port=p, username=u}, _) = (
          addr = a andalso port = p andalso username = u 
        ) 
      in
        if List.exists f serverList
        then continue () 
        else loop (serverCount + 1, serverList@[(si, serverCount)], clientList, calendar, userWithLock, lockQueue)
      end)

    in
      case msg 
        of Request r => (respond r)
         | ServerInfo serverInfo => addServerInfo serverInfo 
         | Client (conn, conn_addr) => (let
                                          val msg = case user of (
                                            User.Info {username, ...}
                                          ) => username 
                                          val dataByteVec = Byte.stringToBytes msg 
                                          val dataSlice = Word8VectorSlice.slice (dataByteVec, 0, NONE)
                                          val _ = Socket.sendVec (conn, dataSlice)
                                          val _  = createServerMsgr (conn, conn_addr) store
                                        in
                                           loop (serverCount, serverList, clientList, calendar, userWithLock, lockQueue)
                                        end)

    end)

    val _ = CML.spawn (fn () => loop (0, [], [], Calendar.calendar (), NONE, []))
  in
    store
  end)

  and createServerMsgr (conn, connAddr) store = (let

    fun loop () = (let

      fun tryLock jsonStr = (let 
        val user = valOf (User.fromString (Json.sGet jsonStr))
        val locked = lockForUser (store, user)
        val _ = (
          if locked
          then Util.sendString (conn, Util.lockGranted)
          else Util.sendString (conn, Util.lockDenied)
        )
      in
        loop ()
      end)

      fun checkAvailable jsonStr = (let 
        val slot = Calendar.parseTimeslot (Json.sGet jsonStr)
        val calendar = getCalendar store

        val _ = (
          if (Calendar.overlap (calendar, slot)) 
          then Util.sendString (conn, Util.notAvailable) 
          else Util.sendString (conn, Util.available) 
        )
      in
        loop ()
      end)

      fun scheduleMeeting jsonStr = (let 

        val slot = Calendar.parseTimeslot (Json.sGet (valOf (Json.objectGet jsonStr Util.timeslot)))

        val users = (case (Json.objectGet jsonStr Util.users)
          of SOME (Json.A jsonList) => map (valOf o User.fromString o Json.sGet) jsonList
           | _ => raise Fail ("should never happen"))

        val _ = addMeeting(store, slot, users)
        val _ = Util.sendString (conn, Util.scheduled)
      in
        loop ()
      end)


      fun releaseLock jsonStr = (let 
        val user = valOf (User.fromString (Json.sGet jsonStr))
        val released = releaseForUser (store, user)
        val _ = (
          if released 
          then Util.sendString (conn, Util.lockReleased)
          else Util.sendString (conn, Util.notReleased)
        )
        val _ = Socket.close conn
      in
        ()
      end)


      fun handleMsg jsonMsg = (let
        val getOp = Json.objectGet jsonMsg 
        val get = valOf o getOp
        val hasKey = isSome o getOp

      in
        if (hasKey Util.requireLock) then (tryLock (get Util.requireLock))
        else if (hasKey Util.checkAvailable) then checkAvailable (get Util.checkAvailable) 
        else if (hasKey Util.scheduleMeeting) then scheduleMeeting (get Util.scheduleMeeting) 
        else if (hasKey Util.releaseLock) then releaseLock (get Util.releaseLock) 
        else loop ()
      end)

      val msgOp = ((SOME (Socket.recvVec (conn, 1024))) handle _ => (
        Socket.close conn; NONE
      ))

    in
      case msgOp 
        of NONE => ()
         | SOME msg => handleMsg (Json.parse (Byte.bytesToString msg))
    end)
  in
    CML.spawn loop 
  end)
end
